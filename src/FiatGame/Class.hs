{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module FiatGame.Class where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.ByteString.Lazy      (toStrict)
import           Data.Maybe
import           Data.Proxy
import           Data.Text                 (Text, pack)
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.UUID                 (toText)
import           Data.UUID.V4              (nextRandom)
import qualified FiatGame.ToClient.Types   as ToClient
import qualified FiatGame.ToServer.Types   as ToServer
import           FiatGame.Types

class
  ( ToJSON s, FromJSON s
  , ToJSON (Move s), FromJSON (Move s)
  , ToJSON (State s), FromJSON (State s)
  , ToJSON (ClientState s), FromJSON (ClientState s)
  , ToJSON (ClientSettings s), FromJSON (ClientSettings s))
  => FiatGame s where

  type State s :: *
  type Move s :: *
  type ClientSettings s :: *
  type ClientState s :: *
  type Environment s :: *

  defaultSettings :: (MonadIO m) => ReaderT (Environment s) m s
  addPlayer :: (MonadIO m) =>FiatPlayer -> s -> ReaderT (Environment s) m (Maybe s)
  initialGameState :: (MonadIO m) => s -> ReaderT (Environment s) m (Either Text (s, GameState (State s) (Move s)))
  makeMove :: (MonadIO m) => FiatPlayer -> s -> GameState (State s) (Move s) -> Move s -> ReaderT (Environment s) m (GameState (State s) (Move s))
  isPlayersTurn :: (MonadIO m) => FiatPlayer -> s -> GameState (State s) (Move s) -> Move s -> ReaderT (Environment s) m Bool
  isMoveValid :: (MonadIO m) => FiatPlayer -> s -> GameState (State s) (Move s) -> Move s -> ReaderT (Environment s) m Bool
  toClientSettingsAndState :: (MonadIO m) => FiatPlayer -> s -> Maybe (GameState (State s) (Move s)) -> ReaderT (Environment s) m (ClientSettings s, Maybe (GameState (ClientState s) (Move s)))

  isCmdAuthorized :: MoveSubmittedBy -> s -> Maybe (GameState (State s) (Move s)) -> ToServer.Msg s (Move s) -> Bool
  isCmdAuthorized (MoveSubmittedBy System) _  _ _ = True
  isCmdAuthorized (MoveSubmittedBy (FiatPlayer p1)) _ _ fc = case ToServer.player fc of
    System          -> False
    (FiatPlayer p2) -> p1 == p2

  fromFiat :: Proxy s -> FiatPlayer -> FromFiat -> ToFiatMsg
  fromFiat _ p ff =
    let (msg :: ToClient.Msg s (State s) (Move s)) = fromFiatToMsg p ff
    in ToFiatMsg $ encodeToText msg

  fromFiatToMsg :: FiatPlayer -> FromFiat -> ToClient.Msg s (State s) (Move s)
  fromFiatToMsg p ff = either (ToClient.Error p .ToClient.DecodeError) id $ mkMsg <$> es <*> megs
    where
      mkMsg = ToClient.Msg (ff^.fromFiatGameHash)
      es = eitherDecodeFromText $ ff^.fromFiatSettings._Wrapped'
      megs = fromMaybe (Right Nothing) $ eitherDecodeFromText <$> ff^?fromFiatGameState._Just._Wrapped'

  gameStateIsOutOfDate :: Proxy s -> FiatPlayer -> Processed
  gameStateIsOutOfDate _ p = Processed (ToFiatMsg $ encodeToText (ToClient.Error p ToClient.GameStateOutOfDate :: ToClient.Msg s (State s) (Move s))) Nothing True

  newHash :: (MonadIO m) => s -> Maybe (GameState (State s) (Move s)) -> ReaderT (Environment s) m FiatGameHash
  newHash _ _ = FiatGameHash . toText <$> liftIO nextRandom

  toClientMsg :: (MonadIO m) => Proxy s -> FiatPlayer -> ToFiatMsg -> ReaderT (Environment s) m ToClientMsg
  toClientMsg _ p (ToFiatMsg etcmsg) = case tcmsg of
    Left err  -> return $ ToClientMsg $ encodeToText (ToClient.Error p $ ToClient.DecodeError err :: ToClient.Msg (ClientSettings s) (State s) (Move s))
    Right e@(ToClient.Error _ _) -> return $ ToClientMsg $ encodeToText e
    Right (ToClient.Msg h s gs) -> do
      (cs,cgs) <- toClientSettingsAndState p s gs
      return $ ToClientMsg $ encodeToText (ToClient.Msg h cs cgs)
    where
      tcmsg :: Either Text (ToClient.Msg s (State s) (Move s))
      tcmsg = eitherDecodeFromText etcmsg

  initialFromFiat :: (MonadIO m) => Proxy s -> FiatPlayer -> ReaderT (Environment s) m (Maybe (FiatGameHash, SettingsMsg))
  initialFromFiat _ p = do
    s :: s <- defaultSettings
    addPlayer p s >>= \case
      Nothing -> pure Nothing
      Just s' -> do
        h <- newHash s' Nothing
        pure $ Just (h, SettingsMsg $ encodeToText s')

  tryAddPlayer :: (MonadIO m) => Proxy s -> FiatPlayer -> SettingsMsg -> ReaderT (Environment s) m (Maybe SettingsMsg)
  tryAddPlayer _ p (SettingsMsg es) = runMaybeT $ do
    s :: s <- MaybeT $ return $ hush $ eitherDecodeFromText es
    added <- MaybeT $ addPlayer p s
    return $ SettingsMsg $ encodeToText added

  processToServer :: (MonadIO m) => Proxy s -> MoveSubmittedBy -> FromFiat -> ToServerMsg -> ReaderT (Environment s) m Processed
  processToServer _ submittedBy@(MoveSubmittedBy mvP) fiat (ToServerMsg ecmsg) = do
    (toChannel ::  Either (FiatPlayer,ToClient.Error) (FiatGameHash, s, Maybe (GameState (State s) (Move s)))) <- runExceptT $ do
      (h, s, mgs) <- ExceptT $ pure $ ToClient.fromMsg $ fromFiatToMsg mvP fiat
      cmsg <- ExceptT $ return $ over _Left (\err -> (mvP,ToClient.DecodeError err)) $ eitherDecodeFromText ecmsg
      ExceptT $ pure $ boolToEither (mvP,ToClient.Unauthorized) $ isCmdAuthorized submittedBy s mgs cmsg
      let p = ToServer.player cmsg
      if h /= ToServer.hash cmsg
      then ExceptT $ return $ Left (mvP,ToClient.GameStateOutOfDate)
      else case ToServer.cmd cmsg of
        ToServer.StartGame                -> case mgs of
          (Just _) -> ExceptT $ return $ Left (mvP,ToClient.GameAlreadyStarted)
          Nothing  -> lift (initialGameState s) >>= \case
            Left err -> ExceptT $ return $ Left (mvP,ToClient.FailedToInitialize err)
            Right (s', gs :: GameState (State s) (Move s)) -> do
              h' <- lift $ newHash s' (Just gs)
              return (h', s', Just gs)
        ToServer.UpdateSettings s'         -> do
          h' <- lift $ newHash s' Nothing
          return (h', s', Nothing)
        ToServer.MakeMove mv -> case mgs of
          Nothing -> ExceptT $ return $ Left (mvP,ToClient.GameIsNotStarted)
          Just gs -> do
            let isSystem = case p of
                            System -> True
                            _      -> False
            ExceptT $ boolToEither (mvP,ToClient.NotYourTurn) . (isSystem ||) <$> isPlayersTurn p s gs mv
            ExceptT $ boolToEither (mvP,ToClient.InvalidMove) <$> isMoveValid p s gs mv
            gs' <- lift $ makeMove p s gs mv
            h' <- lift $ newHash s (Just gs')
            return (h',s, Just gs')
    let msg = ToFiatMsg $ encodeToText $ ToClient.toMsg toChannel
    case toChannel of
      Left (_,ToClient.GameStateOutOfDate)                      -> return $ Processed msg Nothing True
      Left _                      -> return $ Processed msg Nothing False
      Right (h',s, mgs) -> do
        let stage = maybe SettingUp (\(FiatGame.Types.GameState st _ _) -> st) mgs
            fMv :: Maybe (UTCTime, ToServer.Msg s (Move s))
            fMv = fmap (\f -> (f^.futureMoveTime, ToServer.Msg System (ToServer.MakeMove (f^.futureMoveMove)) h')) (join (view gameStateFutureMove <$> mgs))
        return $ Processed msg (Just $ SuccessfulProcessed stage (FromFiat (SettingsMsg (encodeToText s)) (GameStateMsg . encodeToText <$> mgs) h') (over _2 (ToServerMsg . encodeToText) <$> fMv)) False

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
