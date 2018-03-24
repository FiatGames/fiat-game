{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

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
import qualified FiatGame.ToClient.Types   as ToClient
import qualified FiatGame.ToServer.Types   as ToServer
import           FiatGame.Types

class (ToJSON mv, FromJSON mv, ToJSON g, FromJSON g, ToJSON cg, FromJSON cg, ToJSON s, FromJSON s, ToJSON cs, FromJSON cs) => FiatGame g s mv cg cs | s -> mv, s -> g, s -> cg, s -> cs where
  defaultSettings :: IO s
  addPlayer :: FiatPlayer -> s -> IO (Maybe s)
  initialGameState :: s -> IO (Either Text (s, GameState g mv))
  makeMove :: FiatPlayer -> s -> GameState g mv -> mv -> IO (GameState g mv)
  isPlayersTurn :: FiatPlayer -> s -> GameState g mv -> mv -> IO Bool
  isMoveValid :: FiatPlayer -> s -> GameState g mv -> mv -> IO Bool
  toClientSettingsAndState :: FiatPlayer -> s -> Maybe (GameState g mv) -> IO (cs, Maybe (GameState cg mv))
  newHash :: Proxy s -> IO FiatGameHash

  isCmdAuthorized :: MoveSubmittedBy -> s -> Maybe (GameState g mv) -> ToServer.Msg s mv -> IO Bool
  isCmdAuthorized (MoveSubmittedBy System) _  _ _ = return True
  isCmdAuthorized (MoveSubmittedBy (FiatPlayer p1)) _ _ fc = case ToServer.player fc of
    System          -> return False
    (FiatPlayer p2) -> return $ p1 == p2

  toClientMsg :: Proxy s -> FiatPlayer -> ToFiatMsg -> IO ToClientMsg
  toClientMsg _ p (ToFiatMsg etcmsg) = case tcmsg of
    Left err  -> return $ ToClientMsg $ encodeToText (ToClient.Error p $ ToClient.DecodeError err :: ToClient.Msg cs cg mv)
    Right e@(ToClient.Error _ _) -> return $ ToClientMsg $ encodeToText e
    Right (ToClient.Msg h s gs) -> do
      (cs,cgs) <- toClientSettingsAndState p s gs
      return $ ToClientMsg $ encodeToText (ToClient.Msg h cs cgs)
    where
      tcmsg :: Either Text (ToClient.Msg s g mv)
      tcmsg = eitherDecodeFromText etcmsg

  fromFiat :: Proxy s -> FiatPlayer -> FromFiat -> IO ToFiatMsg
  fromFiat _ p ff =  do
    (msg :: ToClient.Msg s g mv) <- fromFiatToMsg p ff
    return $ ToFiatMsg $ encodeToText msg

  fromFiatToMsg :: FiatPlayer -> FromFiat -> IO (ToClient.Msg s g mv)
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
  gameStateIsOutOfDate _ p = return $ Processed (ToFiatMsg $ encodeToText (ToClient.Error p ToClient.GameStateOutOfDate :: ToClient.Msg s g mv)) Nothing

  processToServer :: Proxy s -> MoveSubmittedBy -> FromFiat -> ToServerMsg -> IO Processed
  processToServer _ submittedBy@(MoveSubmittedBy mvP) fiat (ToServerMsg ecmsg) = do
    (toChannel ::  Either (FiatPlayer,ToClient.Error) (FiatGameHash, s, Maybe (GameState g mv))) <- runExceptT $ do
      (h, s, mgs) <- ExceptT $ ToClient.fromMsg <$> fromFiatToMsg mvP fiat
      cmsg <- ExceptT $ return $ over _Left (\err -> (mvP,ToClient.DecodeError err)) $ eitherDecodeFromText ecmsg
      ExceptT $ boolToEither (mvP,ToClient.Unauthorized) <$> isCmdAuthorized submittedBy s mgs cmsg
      h' <- lift $ newHash (Proxy :: Proxy s)
      let p = ToServer.player cmsg
      if h /= ToServer.hash cmsg
      then ExceptT $ return $ Left (mvP,ToClient.GameStateOutOfDate)
      else case ToServer.cmd cmsg of
        ToServer.StartGame                -> case mgs of
          (Just _) -> ExceptT $ return $ Left (mvP,ToClient.GameAlreadyStarted)
          Nothing  -> lift (initialGameState s) >>= \case
            Left err -> ExceptT $ return $ Left (mvP,ToClient.FailedToInitialize err)
            Right (s', gs :: GameState g mv) -> return (h',s', Just gs)
        ToServer.UpdateSettings s'         -> return (h', s', Nothing)
        ToServer.MakeMove mv -> case mgs of
          Nothing -> ExceptT $ return $ Left (mvP,ToClient.GameIsNotStarted)
          Just gs -> do
            let isSystem = case p of
                            System -> True
                            _      -> False
            ExceptT $ boolToEither (mvP,ToClient.NotYourTurn) . (isSystem ||) <$> isPlayersTurn p s gs mv
            ExceptT $ boolToEither (mvP,ToClient.InvalidMove) <$> isMoveValid p s gs mv
            gs' <- lift $ makeMove p s gs mv
            return (h',s, Just gs')
    let msg = ToFiatMsg $ encodeToText $ ToClient.toMsg toChannel
    case toChannel of
      Left _                      -> return $ Processed msg Nothing
      Right (h',s, mgs) -> do
        let stage = maybe SettingUp (\(FiatGame.Types.GameState st _ _) -> st) mgs
            fMv :: Maybe (UTCTime, ToServer.Msg s mv)
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
