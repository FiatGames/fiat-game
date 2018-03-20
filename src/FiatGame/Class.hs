{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

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
import           Data.Time.Clock
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

newtype ToChannelMsg = ToChannelMsg ByteString
  deriving (Eq,Show)

data FromFiat = FromFiat
  { _fromFiatSettings  :: SettingsMsg
  , _fromFiatGameState :: Maybe GameStateMsg
  } deriving (Eq,Show,Generic)
makeLenses ''FromFiat

type ToChannel s g mv = Either (FiatPlayer, ToClient.Error) (SettingsAndState s g mv)

data SuccessfulProcess = SuccessfulProcess
  { _successfulProcessGameStage  :: GameStage
  , _successfulProccessFromFiat  :: FromFiat
  , _successfulProcessFutureMove :: Maybe (UTCTime,FutureMoveMsg)
  }
  deriving (Eq,Show,Generic)
makeLenses ''SuccessfulProcess

data Processed = Processed
  { _processedToChannelMsg :: ToChannelMsg
  , _processedSuccessFul   :: Maybe SuccessfulProcess
  }
  deriving (Eq,Show,Generic)
makeLenses ''Processed
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

  toSettingsAndState :: FiatPlayer -> FromFiat -> m (ToChannel s g mv)
  toSettingsAndState p ff = return $ over _Left (\e -> (p,ToClient.DecodeError e)) $ SettingsAndState <$> eitherDecodeFromText (getSettingsMsg $ ff^.fromFiatSettings) <*> megs'
    where
      megs' = fromMaybe (Right Nothing) (eitherDecodeFromText . getGameStateMsg <$> ff^.fromFiatGameState)

  initialFromFiat :: Proxy s -> FiatPlayer -> m SettingsMsg
  initialFromFiat _ p = do
    s :: s <- defaultSettings
    (Just s') <- addPlayer p s
    return $ SettingsMsg $ encodeToText s'

  fromFiat :: Proxy s -> FiatPlayer -> FromFiat -> m ToChannelMsg
  fromFiat _ p f = ToChannelMsg . toStrict . encode <$> (toSettingsAndState p f :: m (ToChannel s g mv))

  tryAddPlayer :: Proxy s -> FiatPlayer -> SettingsMsg -> m (Maybe SettingsMsg)
  tryAddPlayer _ p (SettingsMsg es) = runMaybeT $ do
    s :: s <- MaybeT $ return $ hush $ eitherDecodeFromText es
    added <- MaybeT $ addPlayer p s
    return $ SettingsMsg $ encodeToText added

  toClientMsg :: Proxy s -> FiatPlayer -> ToChannelMsg -> m ToClientMsg
  toClientMsg _ p (ToChannelMsg echanMsg) = case decoded of
      Left err -> return $ ToClientMsg $ encodeToText (ToClient.Error p (ToClient.DecodeError (pack err)) :: ToClient.Msg cs cg mv)
      Right (Left err) -> return $ ToClientMsg $ encodeToText (uncurry ToClient.Error err :: ToClient.Msg cs cg mv)
      Right (Right s) -> ToClientMsg . encodeToText . ToClient.Msg <$> toClientSettingsAndState p s
    where
      decoded :: Either String (ToChannel s g mv)
      decoded = eitherDecodeStrict echanMsg

  gameStateIsOutOfDate :: Proxy s -> FiatPlayer -> m ToChannelMsg
  gameStateIsOutOfDate _ p = return $ ToChannelMsg $ toStrict $ encode (Left (p, ToClient.GameStateOutOfDate) :: ToChannel s g mv)

  proccessFutureMove :: Proxy s -> FromFiat -> FutureMoveMsg -> m Processed
  proccessFutureMove p f (FutureMoveMsg emsg) = case msg of
      Left err  -> return $ Processed (ToChannelMsg $ toStrict $ encode $ processed err) Nothing
      Right (FutureMove _ mv) -> processToServer p (MoveSubmittedBy System) f (ToServerMsg $ encodeToText $ toServer mv)
    where
      toServer :: mv -> ToServer.Msg s mv
      toServer mv = ToServer.Msg System (ToServer.MakeMove mv)
      processed :: ToClient.Error -> ToChannel s g mv
      processed err = Left (System, err)
      msg :: Either ToClient.Error (FutureMove mv)
      msg = over _Left ToClient.DecodeError $ eitherDecodeFromText emsg

  processToServer :: Proxy s -> MoveSubmittedBy -> FromFiat -> ToServerMsg -> m Processed
  processToServer _ submittedBy@(MoveSubmittedBy mvP) fromFiat (ToServerMsg ecmsg) = do
    (toChannel :: ToChannel s g mv) <- runExceptT $ do
      (SettingsAndState s mgs) <- ExceptT $ toSettingsAndState mvP fromFiat
      cmsg <- ExceptT $ return $ over _Left (\err -> (mvP,ToClient.DecodeError err)) $ eitherDecodeFromText ecmsg
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
    let msg = ToChannelMsg $ toStrict $ encode toChannel
    case toChannel of
      Left _                      -> return $ Processed msg Nothing
      Right (SettingsAndState s mgs) -> do
        let stage = maybe SettingUp (\(FiatGame.GameState.GameState st _ _) -> st) mgs
            fMv = fmap (\future -> (timeForFutureMove future, future)) (join (futureMove <$> mgs))
        return $ Processed msg (Just $ SuccessfulProcess stage (FromFiat (SettingsMsg (encodeToText s)) (GameStateMsg . encodeToText <$> mgs)) (over _2 (FutureMoveMsg . encodeToText) <$> fMv))

boolToEither :: a -> Bool -> Either a ()
boolToEither _ True  = Right ()
boolToEither a False = Left a

hush :: Either a b -> Maybe b
hush (Left _)  = Nothing
hush (Right b) = Just b

encodeToText :: (ToJSON a) => a -> Text
encodeToText = decodeUtf8 . toStrict . encode
eitherDecodeFromText :: (FromJSON a) => Text -> Either Text a
eitherDecodeFromText = over _Left pack . eitherDecodeStrict . encodeUtf8
