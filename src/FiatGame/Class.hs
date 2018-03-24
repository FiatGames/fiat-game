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

  defaultSettings :: IO s
  addPlayer :: FiatPlayer -> s -> IO (Maybe s)
  initialGameState :: s -> IO (Either Text (s, GameState (State s) (Move s)))
  makeMove :: FiatPlayer -> s -> GameState (State s) (Move s) -> Move s -> IO (GameState (State s) (Move s))
  isPlayersTurn :: FiatPlayer -> s -> GameState (State s) (Move s) -> Move s -> IO Bool
  isMoveValid :: FiatPlayer -> s -> GameState (State s) (Move s) -> Move s -> IO Bool
  toClientSettingsAndState :: FiatPlayer -> s -> Maybe (GameState (State s) (Move s)) -> IO (ClientSettings s, Maybe (GameState (ClientState s) (Move s)))

  newHash :: s -> Maybe (GameState (State s) (Move s)) -> IO FiatGameHash
  newHash _ _ = FiatGameHash . toText <$> nextRandom

  isCmdAuthorized :: MoveSubmittedBy -> s -> Maybe (GameState (State s) (Move s)) -> ToServer.Msg s (Move s) -> IO Bool
  isCmdAuthorized (MoveSubmittedBy System) _  _ _ = return True
  isCmdAuthorized (MoveSubmittedBy (FiatPlayer p1)) _ _ fc = case ToServer.player fc of
    System          -> return False
    (FiatPlayer p2) -> return $ p1 == p2

  toClientMsg :: Proxy s -> FiatPlayer -> ToFiatMsg -> IO ToClientMsg
  toClientMsg _ p (ToFiatMsg etcmsg) = case tcmsg of
    Left err  -> return $ ToClientMsg $ encodeToText (ToClient.Error p $ ToClient.DecodeError err :: ToClient.Msg (ClientSettings s) (State s) (Move s))
    Right e@(ToClient.Error _ _) -> return $ ToClientMsg $ encodeToText e
    Right (ToClient.Msg h s gs) -> do
      (cs,cgs) <- toClientSettingsAndState p s gs
      return $ ToClientMsg $ encodeToText (ToClient.Msg h cs cgs)
    where
      tcmsg :: Either Text (ToClient.Msg s (State s) (Move s))
      tcmsg = eitherDecodeFromText etcmsg

  fromFiat :: Proxy s -> FiatPlayer -> FromFiat -> IO ToFiatMsg
  fromFiat _ p ff =  do
    (msg :: ToClient.Msg s (State s) (Move s)) <- fromFiatToMsg p ff
    return $ ToFiatMsg $ encodeToText msg

  fromFiatToMsg :: FiatPlayer -> FromFiat -> IO (ToClient.Msg s (State s) (Move s))
  fromFiatToMsg p ff = return $ either (ToClient.Error p .ToClient.DecodeError) id $ mkMsg <$> es <*> megs
    where
      mkMsg = ToClient.Msg (ff^.fromFiatGameHash)
      es = eitherDecodeFromText $ ff^.fromFiatSettings._Wrapped'
      megs = fromMaybe (Right Nothing) $ eitherDecodeFromText <$> ff^?fromFiatGameState._Just._Wrapped'

  initialFromFiat :: Proxy s -> FiatPlayer -> IO SettingsMsg
  initialFromFiat _ p = do
    s :: s <- defaultSettings
    (Just s') <- addPlayer p s
    return $ SettingsMsg $ encodeToText s'

  tryAddPlayer :: Proxy s -> FiatPlayer -> SettingsMsg -> IO (Maybe SettingsMsg)
  tryAddPlayer _ p (SettingsMsg es) = runMaybeT $ do
    s :: s <- MaybeT $ return $ hush $ eitherDecodeFromText es
    added <- MaybeT $ addPlayer p s
    return $ SettingsMsg $ encodeToText added

  gameStateIsOutOfDate :: Proxy s -> FiatPlayer -> IO Processed
  gameStateIsOutOfDate _ p = return $ Processed (ToFiatMsg $ encodeToText (ToClient.Error p ToClient.GameStateOutOfDate :: ToClient.Msg s (State s) (Move s))) Nothing

  processToServer :: Proxy s -> MoveSubmittedBy -> FromFiat -> ToServerMsg -> IO Processed
  processToServer _ submittedBy@(MoveSubmittedBy mvP) fiat (ToServerMsg ecmsg) = do
    (toChannel ::  Either (FiatPlayer,ToClient.Error) (FiatGameHash, s, Maybe (GameState (State s) (Move s)))) <- runExceptT $ do
      (h, s, mgs) <- ExceptT $ ToClient.fromMsg <$> fromFiatToMsg mvP fiat
      cmsg <- ExceptT $ return $ over _Left (\err -> (mvP,ToClient.DecodeError err)) $ eitherDecodeFromText ecmsg
      ExceptT $ boolToEither (mvP,ToClient.Unauthorized) <$> isCmdAuthorized submittedBy s mgs cmsg
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
      Left _                      -> return $ Processed msg Nothing
      Right (h',s, mgs) -> do
        let stage = maybe SettingUp (\(FiatGame.Types.GameState st _ _) -> st) mgs
            fMv :: Maybe (UTCTime, ToServer.Msg s (Move s))
            fMv = fmap (\f -> (f^.futureMoveTime, ToServer.Msg System (ToServer.MakeMove (f^.futureMoveMove)) h')) (join (futureMove <$> mgs))
        return $ Processed msg (Just $ SuccessfulProcessed stage (FromFiat (SettingsMsg (encodeToText s)) (GameStateMsg . encodeToText <$> mgs) h') (over _2 (ToServerMsg . encodeToText) <$> fMv))

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
