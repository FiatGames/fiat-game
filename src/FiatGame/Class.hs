{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

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
import           GHC.Generics              hiding (from)

newtype SettingsMsg = SettingsMsg { getSettingsMsg :: Text }
  deriving (Eq,Show,Generic)
makeWrapped ''SettingsMsg

newtype GameStateMsg = GameStateMsg { getGameStateMsg :: Text }
  deriving (Eq,Show,Generic)
makeWrapped ''GameStateMsg

newtype ToServerMsg = ToServerMsg { getToServerMsg :: Text }
  deriving (Eq,Show,Generic)
makeWrapped ''ToServerMsg

newtype ToFiatMsg = ToFiatMsg { getToFiatMsg :: Text }
  deriving (Eq,Show,Generic)
makeWrapped ''ToFiatMsg

newtype ToClientMsg = ToClientMsg { getToClientMsg :: Text }
  deriving (Eq,Show,Generic)
makeWrapped ''ToClientMsg

newtype FutureMoveMsg = FutureMoveMsg { getFutureMoveMsg :: Text }
  deriving (Eq,Show,Generic)
makeWrapped ''FutureMoveMsg

newtype MoveSubmittedBy = MoveSubmittedBy { getSubmittedBy :: FiatPlayer }
  deriving (Eq,Show,Generic)
makeWrapped ''MoveSubmittedBy

data FromFiat = FromFiat
  { _fromFiatSettings  :: SettingsMsg
  , _fromFiatGameState :: Maybe GameStateMsg
  , _fromFiatGameHash  :: FiatGameHash
  } deriving (Eq,Show,Generic)
makeLenses ''FromFiat

--type ToChannel s g mv = Either (FiatPlayer, ToClient.Error) (SettingsAndState s g mv)

data SuccessfulProcessed = SuccessfulProcessed
  { _successfulProcessedGameStage  :: GameStage
  , _successfulProccessedFromFiat  :: FromFiat
  , _successfulProcessedFutureMove :: Maybe (UTCTime,FutureMoveMsg)
  }
  deriving (Eq,Show,Generic)
makeLenses ''SuccessfulProcessed

data Processed = Processed
  { _processedToClientMsg :: ToFiatMsg
  , _processedSuccessFul  :: Maybe SuccessfulProcessed
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
  newHash :: s -> m FiatGameHash

  isCmdAuthorized :: MoveSubmittedBy -> SettingsAndState s g mv -> ToServer.Msg s mv -> m Bool
  isCmdAuthorized (MoveSubmittedBy System) _  _ = return True
  isCmdAuthorized (MoveSubmittedBy (FiatPlayer p1)) _ fc = case ToServer.player fc of
    System          -> return False
    (FiatPlayer p2) -> return $ p1 == p2

  toClientMsg :: Proxy s -> FiatPlayer -> ToFiatMsg -> m ToClientMsg
  toClientMsg _ p (ToFiatMsg etcmsg) = case tcmsg of
    Left err  -> return $ ToClientMsg $ encodeToText (ToClient.Error p $ ToClient.DecodeError err :: ToClient.Msg cs cg mv)
    Right e@(ToClient.Error _ _) -> return $ ToClientMsg $ encodeToText e
    Right (ToClient.Msg h s) -> do
      s' <- toClientSettingsAndState p s
      return $ ToClientMsg $ encodeToText (ToClient.Msg h s')
    where
      tcmsg :: Either Text (ToClient.Msg s g mv)
      tcmsg = eitherDecodeFromText etcmsg

  fromFiat :: Proxy s -> FiatPlayer -> FromFiat -> m ToFiatMsg
  fromFiat _ p ff =  do
    (msg :: ToClient.Msg s g mv) <- fromFiatToMsg p ff
    return $ ToFiatMsg $ encodeToText msg

  fromFiatToMsg :: FiatPlayer -> FromFiat -> m (ToClient.Msg s g mv)
  fromFiatToMsg p ff = return $ mkMsg $ SettingsAndState <$> es <*> megs
    where
      mkMsg (Left err) = ToClient.Error p $ ToClient.DecodeError err
      mkMsg (Right s)  = ToClient.Msg (ff^.fromFiatGameHash) s
      es = eitherDecodeFromText $ ff^.fromFiatSettings._Wrapped'
      megs = fromMaybe (Right Nothing) $ eitherDecodeFromText <$> ff^?fromFiatGameState._Just._Wrapped'

  initialFromFiat :: Proxy s -> FiatPlayer -> m SettingsMsg
  initialFromFiat _ p = do
    s :: s <- defaultSettings
    (Just s') <- addPlayer p s
    return $ SettingsMsg $ encodeToText s'

  tryAddPlayer :: Proxy s -> FiatPlayer -> SettingsMsg -> m (Maybe SettingsMsg)
  tryAddPlayer _ p (SettingsMsg es) = runMaybeT $ do
    s :: s <- MaybeT $ return $ hush $ eitherDecodeFromText es
    added <- MaybeT $ addPlayer p s
    return $ SettingsMsg $ encodeToText added

  gameStateIsOutOfDate :: Proxy s -> FiatPlayer -> m ToFiatMsg
  gameStateIsOutOfDate _ p = return $ ToFiatMsg $ encodeToText (ToClient.Error p ToClient.GameStateOutOfDate :: ToClient.Msg s g mv)

  proccessFutureMove :: Proxy s -> FromFiat -> FutureMoveMsg -> m Processed
  proccessFutureMove p f (FutureMoveMsg emsg) = case msg of
      Left err  -> return $ Processed (ToFiatMsg $ encodeToText $ mkErr err) Nothing
      Right fmv -> case eitherDecodeFromText (f^.fromFiatSettings._Wrapped') of
        Left err -> return $ Processed (ToFiatMsg $ encodeToText $ mkErr err) Nothing
        Right (_ :: s) -> processToServer p (MoveSubmittedBy System) f (ToServerMsg $ encodeToText $ toServer fmv)
    where
      toServer :: FutureMove mv -> ToServer.Msg s mv
      toServer fmv = ToServer.Msg System (ToServer.MakeMove (fmv^.futureMoveMove)) (fmv^.futureMoveHash)
      mkErr :: Text -> ToClient.Msg s g mv
      mkErr =  ToClient.Error System . ToClient.DecodeError
      msg :: Either Text (FutureMove mv)
      msg = eitherDecodeFromText emsg

  processToServer :: Proxy s -> MoveSubmittedBy -> FromFiat -> ToServerMsg -> m Processed
  processToServer _ submittedBy@(MoveSubmittedBy mvP) fiat (ToServerMsg ecmsg) = do
    (toChannel ::  Either (FiatPlayer,ToClient.Error) (FiatGameHash, SettingsAndState s g mv)) <- runExceptT $ do
      (h, SettingsAndState s mgs) <- ExceptT $ ToClient.fromMsg <$> fromFiatToMsg mvP fiat
      cmsg <- ExceptT $ return $ over _Left (\err -> (mvP,ToClient.DecodeError err)) $ eitherDecodeFromText ecmsg
      ExceptT $ boolToEither (mvP,ToClient.Unauthorized) <$> isCmdAuthorized submittedBy (SettingsAndState s mgs) cmsg
      h' <- lift $ newHash s
      let p = ToServer.player cmsg
      if h /= ToServer.hash cmsg
      then ExceptT $ return $ Left (mvP,ToClient.GameAlreadyStarted)
      else case ToServer.cmd cmsg of
        ToServer.StartGame                -> case mgs of
          (Just _) -> ExceptT $ return $ Left (mvP,ToClient.GameAlreadyStarted)
          Nothing  -> lift (initialGameState s) >>= \case
            Left err -> ExceptT $ return $ Left (mvP,ToClient.FailedToInitialize err)
            Right (s', gs :: GameState g mv) -> return (h',SettingsAndState s' (Just gs))
        ToServer.UpdateSettings s'         -> return (h', SettingsAndState s' Nothing)
        ToServer.MakeMove mv -> case mgs of
          Nothing -> ExceptT $ return $ Left (mvP,ToClient.GameIsNotStarted)
          Just gs -> do
            let isSystem = case p of
                            System -> True
                            _      -> False
            ExceptT $ boolToEither (mvP,ToClient.NotYourTurn) . (isSystem ||) <$> isPlayersTurn p s gs mv
            ExceptT $ boolToEither (mvP,ToClient.InvalidMove) <$> isMoveValid p s gs mv
            gs' <- lift $ makeMove p s gs mv
            return (h',SettingsAndState s (Just gs'))
    let msg = ToFiatMsg $ encodeToText $ ToClient.toMsg toChannel
    case toChannel of
      Left _                      -> return $ Processed msg Nothing
      Right (h',SettingsAndState s mgs) -> do
        let stage = maybe SettingUp (\(FiatGame.GameState.GameState st _ _) -> st) mgs
            fMv = fmap (\f -> (f^.futureMoveTime, f)) (join (futureMove <$> mgs))
        return $ Processed msg (Just $ SuccessfulProcessed stage (FromFiat (SettingsMsg (encodeToText s)) (GameStateMsg . encodeToText <$> mgs) h') (over _2 (FutureMoveMsg . encodeToText) <$> fMv))

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
