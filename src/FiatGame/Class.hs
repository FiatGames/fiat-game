{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module FiatGame.Class (FiatGame(..), SettingsMsg(..), GameStateMsg(..), ToServerMsg(..), MoveSubmittedBy(..), ChannelMsg(..), FromFiat) where

import           Control.Lens
import           Control.Monad.Except
import           Data.Aeson
import           Data.ByteString         (ByteString)
import           Data.ByteString.Lazy    (toStrict)
import           Data.Maybe
import           Data.Proxy
import           Data.Text               (Text, pack)
import           Data.Text.Encoding
import           FiatGame.GameState
import qualified FiatGame.ToClient.Types as ToClient
import qualified FiatGame.ToServer.Types as ToServer
import           GHC.Generics

newtype SettingsMsg = SettingsMsg { getSettingsMsg :: Text }
  deriving (Eq,Show,Generic)

newtype GameStateMsg = GameStateMsg { getGameStateMsg :: Text }
  deriving (Eq,Show,Generic)

newtype ToServerMsg = ToServerMsg { getToServerMsg :: Text }
  deriving (Eq,Show,Generic)

newtype MoveSubmittedBy = MoveSubmittedBy { getSubmittedBy :: FiatPlayer }
  deriving (Eq,Show,Generic)

newtype ChannelMsg = ChannelMsg ByteString
  deriving (Eq,Show)

type FromFiat = (SettingsMsg, Maybe GameStateMsg)
type Processed s g mv = Either ToClient.Error (SettingsAndState s g mv)

class (Monad m, ToJSON mv, FromJSON mv, ToJSON g, FromJSON g, ToJSON cg, FromJSON cg, ToJSON s, FromJSON s, ToJSON cs, FromJSON cs) => FiatGame m g s mv cg cs | s -> mv, s -> g, s -> cg, s -> cs where
  defaultSettings :: m s
  addFiatPlayer :: FiatPlayer -> s -> m (Maybe s)
  initialGameState :: s -> m (Either Text (s, GameState g mv))
  makeMove :: FiatPlayer -> s -> GameState g mv -> mv -> m (GameState g mv)
  isFiatPlayersTurn :: FiatPlayer -> s -> GameState g mv -> mv -> m Bool
  isMoveValid :: FiatPlayer -> s -> GameState g mv -> mv -> m Bool
  toClientSettingsAndState :: FiatPlayer -> SettingsAndState s g mv -> m (SettingsAndState cs cg mv)

  isCmdAuthorized :: MoveSubmittedBy -> SettingsAndState s g mv -> ToServer.Msg s mv -> m Bool
  isCmdAuthorized (MoveSubmittedBy System) _  _ = return True
  isCmdAuthorized (MoveSubmittedBy (FiatPlayer p1)) _ fc = case ToServer.player fc of
    System          -> return False
    (FiatPlayer p2) -> return $ p1 == p2

  toSettingsAndState :: FromFiat -> m (Processed s g mv)
  toSettingsAndState (SettingsMsg es,megs) = return $ over _Left ToClient.DecodeError $ SettingsAndState <$> es' <*> megs'
    where
      es' = over _Left pack <$> eitherDecodeStrict $ encodeUtf8 es
      megs' = fromMaybe (Right Nothing) $ over _Left pack <$> (eitherDecodeStrict . encodeUtf8 . getGameStateMsg <$> megs)

  toGameChannelMsg :: Processed s g mv -> m ChannelMsg
  toGameChannelMsg = return . ChannelMsg . toStrict . encode

  toClientMsg :: Proxy s -> FiatPlayer -> ChannelMsg -> m Text
  toClientMsg _ p (ChannelMsg echanMsg) = case decoded of
      Left err -> return $ decodeUtf8 $ toStrict $ encode (ToClient.Error (ToClient.DecodeError (pack err)) :: ToClient.Msg cs cg mv)
      Right (Left err) -> return $ decodeUtf8 $ toStrict $ encode (ToClient.Error err :: ToClient.Msg cs cg mv)
      Right (Right s) -> decodeUtf8 . toStrict . encode . ToClient.Msg <$> toClientSettingsAndState p s
    where
      decoded :: Either String (Processed s g mv)
      decoded = eitherDecodeStrict echanMsg

  processToServer :: Proxy s -> MoveSubmittedBy -> FromFiat -> ToServerMsg -> m (ChannelMsg, Maybe (GameStage,FromFiat))
  processToServer _ submittedBy fromFiat (ToServerMsg ecmsg) = do
    (processed :: Processed s g mv) <- runExceptT $ do
      (SettingsAndState s mgs) <- ExceptT $ toSettingsAndState fromFiat
      cmsg <- ExceptT $ return $ over _Left (ToClient.DecodeError . pack) $ eitherDecodeStrict $ encodeUtf8 ecmsg
      ExceptT $ boolToEither ToClient.Unauthorized <$> isCmdAuthorized submittedBy (SettingsAndState s mgs) cmsg
      let p = ToServer.player cmsg
      case ToServer.cmd cmsg of
        ToServer.StartGame                -> case mgs of
          (Just _) -> ExceptT $ return $ Left ToClient.GameAlreadyStarted
          Nothing  -> lift (initialGameState s) >>= \case
            Left err -> ExceptT $ return $ Left $ ToClient.FailedToInitialize err
            Right (s', gs :: GameState g mv) -> ExceptT $ return $ Right $ SettingsAndState s' (Just gs)
        ToServer.UpdateSettings s'         -> ExceptT $ return $ Right $ SettingsAndState s' Nothing
        ToServer.MakeMove mv -> case mgs of
          Nothing -> ExceptT $ return $ Left ToClient.GameIsNotStarted
          Just gs -> do
            let isSystem = case p of
                            System -> True
                            _      -> False
            ExceptT $ boolToEither ToClient.NotYourTurn . (isSystem ||) <$> isFiatPlayersTurn p s gs mv
            ExceptT $ boolToEither ToClient.InvalidMove <$> isMoveValid p s gs mv
            gs' <- lift $ makeMove p s gs mv
            return $ SettingsAndState s (Just gs')
    msg <- toGameChannelMsg processed
    case processed of
      Left _                      -> return (msg,Nothing)
      Right (SettingsAndState s mgs) -> do
        let stage = maybe SettingUp (\(FiatGame.GameState.GameState st _ _) -> st) mgs
        return (msg, Just (stage, (SettingsMsg (decodeUtf8 $ toStrict $ encode s), GameStateMsg . decodeUtf8 . toStrict . encode <$> mgs)))

boolToEither :: a -> Bool -> Either a ()
boolToEither _ True  = Right ()
boolToEither a False = Left a

