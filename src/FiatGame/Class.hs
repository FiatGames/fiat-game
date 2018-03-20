{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module FiatGame.Class where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.ByteString           (ByteString)
import           Data.ByteString.Lazy      (toStrict)
import           Data.Maybe
import           Data.Proxy
import           Data.Text                 (Text, pack)
import           Data.Text.Encoding
import           FiatGame.GameState
import qualified FiatGame.ToClient.Types   as ToClient
import qualified FiatGame.ToServer.Types   as ToServer
import           GHC.Generics

newtype SettingsMsg = SettingsMsg { getSettingsMsg :: Text }
  deriving (Eq,Show,Generic)

newtype GameStateMsg = GameStateMsg { getGameStateMsg :: Text }
  deriving (Eq,Show,Generic)

newtype ToServerMsg = ToServerMsg { getToServerMsg :: Text }
  deriving (Eq,Show,Generic)

newtype ToClientMsg = ToClientMsg { getToClientMsg :: Text }
  deriving (Eq,Show,Generic)

newtype FutureMoveMsg = FutureMoveMsg { getFutureMoveMsg :: Text }
  deriving (Eq,Show,Generic)

newtype MoveSubmittedBy = MoveSubmittedBy { getSubmittedBy :: FiatPlayer }
  deriving (Eq,Show,Generic)

newtype ChannelMsg = ChannelMsg ByteString
  deriving (Eq,Show)

type FromFiat = (SettingsMsg, Maybe GameStateMsg)
type Processed s g mv = Either (FiatPlayer, ToClient.Error) (SettingsAndState s g mv)

class (Monad m, ToJSON mv, FromJSON mv, ToJSON g, FromJSON g, ToJSON cg, FromJSON cg, ToJSON s, FromJSON s, ToJSON cs, FromJSON cs) => FiatGame m g s mv cg cs | s -> mv, s -> g, s -> cg, s -> cs where
  defaultSettings :: m s
  addPlayer :: FiatPlayer -> s -> m (Maybe s)
  initialGameState :: s -> m (Either Text (s, GameState g mv))
  makeMove :: FiatPlayer -> s -> GameState g mv -> mv -> m (GameState g mv)
  isPlayersTurn :: FiatPlayer -> s -> GameState g mv -> mv -> m Bool
  isMoveValid :: FiatPlayer -> s -> GameState g mv -> mv -> m Bool
  toClientSettingsAndState :: FiatPlayer -> SettingsAndState s g mv -> m (SettingsAndState cs cg mv)

  isCmdAuthorized :: MoveSubmittedBy -> SettingsAndState s g mv -> ToServer.Msg s mv -> m Bool
  isCmdAuthorized (MoveSubmittedBy System) _  _ = return True
  isCmdAuthorized (MoveSubmittedBy (FiatPlayer p1)) _ fc = case ToServer.player fc of
    System          -> return False
    (FiatPlayer p2) -> return $ p1 == p2

  toSettingsAndState :: FiatPlayer -> FromFiat -> m (Processed s g mv)
  toSettingsAndState p (SettingsMsg es,megs) = return $ over _Left (\e -> (p,ToClient.DecodeError e)) $ SettingsAndState <$> es' <*> megs'
    where
      es' = over _Left pack <$> eitherDecodeStrict $ encodeUtf8 es
      megs' = fromMaybe (Right Nothing) $ over _Left pack <$> (eitherDecodeStrict . encodeUtf8 . getGameStateMsg <$> megs)

  initialFromFiat :: Proxy s -> FiatPlayer -> m SettingsMsg
  initialFromFiat _ p = do
    s :: s <- defaultSettings
    (Just s') <- addPlayer p s
    return $ SettingsMsg $ decodeUtf8 $ toStrict $ encode s'

  fromFiat :: Proxy s -> FiatPlayer -> FromFiat -> m ChannelMsg
  fromFiat _ p f = ChannelMsg . toStrict . encode <$> (toSettingsAndState p f :: m (Processed s g mv))

  tryAddPlayer :: Proxy s -> FiatPlayer -> SettingsMsg -> m (Maybe SettingsMsg)
  tryAddPlayer _ p (SettingsMsg es) = runMaybeT $ do
    s :: s <- MaybeT $ return $ decodeStrict $ encodeUtf8 es
    added <- MaybeT $ addPlayer p s
    return $ SettingsMsg $ decodeUtf8 $ toStrict $ encode added

  toClientMsg :: Proxy s -> FiatPlayer -> ChannelMsg -> m ToClientMsg
  toClientMsg _ p (ChannelMsg echanMsg) = case decoded of
      Left err -> return $ ToClientMsg . decodeUtf8 $ toStrict $ encode (ToClient.Error p (ToClient.DecodeError (pack err)) :: ToClient.Msg cs cg mv)
      Right (Left err) -> return $ ToClientMsg .decodeUtf8 $ toStrict $ encode (uncurry ToClient.Error err :: ToClient.Msg cs cg mv)
      Right (Right s) -> ToClientMsg . decodeUtf8 . toStrict . encode . ToClient.Msg <$> toClientSettingsAndState p s
    where
      decoded :: Either String (Processed s g mv)
      decoded = eitherDecodeStrict echanMsg

  gameStateIsOutOfDate :: Proxy s -> FiatPlayer -> m ChannelMsg
  gameStateIsOutOfDate _ p = return $ ChannelMsg $ toStrict $ encode (Left (p, ToClient.GameStateOutOfDate) :: Processed s g mv)

  processToServer :: Proxy s -> MoveSubmittedBy -> FromFiat -> ToServerMsg -> m (ChannelMsg, Maybe (GameStage,FromFiat,Maybe FutureMoveMsg))
  processToServer _ submittedBy@(MoveSubmittedBy mvP) f (ToServerMsg ecmsg) = do
    (processed :: Processed s g mv) <- runExceptT $ do
      (SettingsAndState s mgs) <- ExceptT $ toSettingsAndState mvP f
      cmsg <- ExceptT $ return $ over _Left (\err -> (mvP,ToClient.DecodeError $ pack err)) $ eitherDecodeStrict $ encodeUtf8 ecmsg
      ExceptT $ boolToEither (mvP,ToClient.Unauthorized) <$> isCmdAuthorized submittedBy (SettingsAndState s mgs) cmsg
      let p = ToServer.player cmsg
      case ToServer.cmd cmsg of
        ToServer.StartGame                -> case mgs of
          (Just _) -> ExceptT $ return $ Left (mvP,ToClient.GameAlreadyStarted)
          Nothing  -> lift (initialGameState s) >>= \case
            Left err -> ExceptT $ return $ Left (mvP,ToClient.FailedToInitialize err)
            Right (s', gs :: GameState g mv) -> ExceptT $ return $ Right $ SettingsAndState s' (Just gs)
        ToServer.UpdateSettings s'         -> ExceptT $ return $ Right $ SettingsAndState s' Nothing
        ToServer.MakeMove mv -> case mgs of
          Nothing -> ExceptT $ return $ Left (mvP,ToClient.GameIsNotStarted)
          Just gs -> do
            let isSystem = case p of
                            System -> True
                            _      -> False
            ExceptT $ boolToEither (mvP,ToClient.NotYourTurn) . (isSystem ||) <$> isPlayersTurn p s gs mv
            ExceptT $ boolToEither (mvP,ToClient.InvalidMove) <$> isMoveValid p s gs mv
            gs' <- lift $ makeMove p s gs mv
            return $ SettingsAndState s (Just gs')
    let msg = ChannelMsg $ toStrict $ encode processed
    case processed of
      Left _                      -> return (msg,Nothing)
      Right (SettingsAndState s mgs) -> do
        let stage = maybe SettingUp (\(FiatGame.GameState.GameState st _ _) -> st) mgs
            fMv = join $ futureMove <$> mgs
        return (msg, Just (stage, (SettingsMsg (decodeUtf8 $ toStrict $ encode s), GameStateMsg . decodeUtf8 . toStrict . encode <$> mgs), FutureMoveMsg . decodeUtf8 . toStrict . encode <$> fMv))

boolToEither :: a -> Bool -> Either a ()
boolToEither _ True  = Right ()
boolToEither a False = Left a
