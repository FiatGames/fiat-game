{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module FiatGame.Class where

import           Control.Lens
import           Control.Monad.Except
import           Data.Aeson
import           Data.Text               (Text, pack)
import           Data.Text.Encoding
import           FiatGame.GameState
import qualified FiatGame.ToClient.Types as ToClient
import qualified FiatGame.ToServer.Types as ToServer
import           GHC.Generics

newtype FiatGameSettingsMsg = FiatGameSettingsMsg {getGameSettingsMsg :: Text}
  deriving (Eq,Show,Generic)

newtype FiatGameStateMsg = FiatGameStateMsg {getGameStateMsg :: Text}
  deriving (Eq,Show,Generic)

newtype FiatToServertMsg = FiatToServertMsg {getToServerMsg :: Text}
  deriving (Eq,Show,Generic)

newtype FiatMoveSubmittedBy = FiatMoveSubmittedBy {getSubmittedBy :: FiatPlayer }
  deriving (Eq,Show,Generic)

class (Monad m, ToJSON mv, FromJSON mv, ToJSON g, FromJSON g, ToJSON cg, FromJSON cg, ToJSON s, FromJSON s, ToJSON cs, FromJSON cs) => FiatGame m g s mv cg cs | s -> mv, s -> g, s -> cg, s -> cs where
  defaultSettings :: m s
  addPlayer :: FiatPlayer -> s -> m (Maybe s)
  initialGameState :: s -> m (Either Text (s, GameState g mv))
  makeMove :: FiatPlayer -> s -> GameState g mv -> mv -> m (GameState g mv)
  isPlayersTurn :: FiatPlayer -> s -> GameState g mv -> mv -> m Bool
  isMoveValid :: FiatPlayer -> s -> GameState g mv -> mv -> m Bool
  toClientSettingsAndState :: FiatPlayer -> SettingsAndState s g mv -> m (SettingsAndState cs cg mv)
  isCmdAuthorized :: FiatMoveSubmittedBy -> SettingsAndState s g mv -> ToServer.Msg s mv -> m Bool
  isCmdAuthorized (FiatMoveSubmittedBy System) _  _ = return True
  isCmdAuthorized (FiatMoveSubmittedBy (FiatPlayer p1)) _ fc = case ToServer.player fc of
    System          -> return False
    (FiatPlayer p2) -> return $ p1 == p2
  toClientMsg :: FiatPlayer -> Either (ToServer.MsgProcessed s g mv) (SettingsAndState s g mv) ->  m (ToClient.Msg cs cg mv)
  toClientMsg _ (Left (ToServer.Error err))   = return $ ToClient.Error err
  toClientMsg p (Left (ToServer.MsgProcessed s))   = ToClient.Msg <$> toClientSettingsAndState p s
  toClientMsg p (Right s) = ToClient.Msg <$> toClientSettingsAndState p s
  processToServer :: FiatMoveSubmittedBy -> FiatGameSettingsMsg -> Maybe FiatGameStateMsg -> FiatToServertMsg -> m (ToServer.MsgProcessed s g mv)
  processToServer submittedBy (FiatGameSettingsMsg es) megs (FiatToServertMsg ecmsg) = fmap ToServer.fromEither $ runExceptT $ do
    s <- ExceptT $ return $ over _Left (ToClient.DecodeError . pack) $ eitherDecodeStrict $ encodeUtf8 es
    cmsg <- ExceptT $ return $ over _Left (ToClient.DecodeError . pack) $ eitherDecodeStrict $ encodeUtf8 ecmsg
    mgs <- case megs of
      Nothing -> ExceptT $ return $ Right Nothing
      (Just (FiatGameStateMsg egs)) -> ExceptT $ return $ over _Left (ToClient.DecodeError . pack) $ eitherDecodeStrict $ encodeUtf8 egs
    ExceptT $ boolToEither ToClient.Unauthorized <$> isCmdAuthorized submittedBy (SettingsAndState s mgs) cmsg
    let p = ToServer.player cmsg
    case ToServer.cmd cmsg of
      ToServer.StartGame                -> case mgs of
        (Just _) -> ExceptT $ return $ Left ToClient.GameAlreadyStarted
        Nothing  -> lift (initialGameState s) >>= \case
          Left err -> ExceptT $ return $ Left $ ToClient.FailedToInitialize err
          Right (s', gs :: GameState g mv) -> ExceptT $ return $ Right (s',Just gs)
      ToServer.UpdateSettings s'         -> ExceptT $ return $ Right (s',Nothing)
      ToServer.MakeMove mv -> case megs of
        Nothing -> ExceptT $ return $ Left ToClient.GameIsNotStarted
        Just (FiatGameStateMsg egs) -> do
          (gs :: GameState g mv) <- ExceptT $ return $ over _Left (ToClient.DecodeError . pack) $ eitherDecodeStrict $ encodeUtf8 egs
          let isSystem = case p of
                          System -> True
                          _      -> False
          ExceptT $ boolToEither ToClient.NotYourTurn . (isSystem ||) <$> isPlayersTurn p s gs mv
          ExceptT $ boolToEither ToClient.InvalidMove <$> isMoveValid p s gs mv
          gs' <- lift $ makeMove p s gs mv
          return (s, Just gs')

boolToEither :: a -> Bool -> Either a ()
boolToEither _ True  = Right ()
boolToEither a False = Left a
