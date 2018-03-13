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

class (Monad m, ToJSON mv, FromJSON mv, ToJSON g, FromJSON g, ToJSON cg, FromJSON cg, ToJSON s, FromJSON s) => FiatGame m g s mv cg | s -> mv, s -> g, s -> cg where
  defaultSettings :: m s
  addPlayer :: FiatPlayer -> s -> m (Maybe s)
  initialGameState :: s -> m (Either Text (s,GameState g mv))
  makeMove :: s -> GameState g mv -> (FiatPlayer, mv) -> m (GameState g mv)
  isPlayersTurn :: s -> GameState g mv -> (FiatPlayer, mv) -> m Bool
  isMoveValid :: s -> GameState g mv -> (FiatPlayer, mv) -> m Bool
  toClientGameState :: FiatPlayer -> s -> GameState g mv -> m (GameState cg mv)
  isCmdAuthorized :: s -> Maybe (GameState g mv) -> FiatPlayer -> ToServer.Msg s mv -> m Bool
  isCmdAuthorized _ _ System _ = return True
  isCmdAuthorized _ _ (FiatPlayer p1) fc = case ToServer.player fc of
    System          -> return False
    (FiatPlayer p2) -> return $ p1 == p2
  toClientMsg :: FiatPlayer -> FiatGameSettingsMsg -> Maybe FiatGameStateMsg -> m (ToClient.Msg s cg mv)
  toClientMsg p (FiatGameSettingsMsg es) megs = do
    let m = do
          s :: s <- eitherDecodeStrict $ encodeUtf8 es
          gs :: Maybe (GameState g mv) <- case megs of
              Nothing    -> Right Nothing
              (Just (FiatGameStateMsg egs)) -> eitherDecodeStrict $ encodeUtf8 egs
          return (s,gs)
    case m of
      Left err -> return $ ToClient.Error $ ToClient.DecodeError $ pack err
      Right (s,Nothing) -> return $ ToClient.Msg s Nothing
      Right (s,Just gs) -> do
                cgs <- toClientGameState p s gs
                return $ ToClient.Msg s (Just cgs)
  processToServer :: FiatPlayer -> FiatGameSettingsMsg -> Maybe FiatGameStateMsg -> FiatToServertMsg -> m (ToClient.Msg s cg mv)
  processToServer p (FiatGameSettingsMsg es) megs (FiatToServertMsg ecmsg) = fmap ToClient.fromEither $ runExceptT $ do
    s <- ExceptT $ return $ over _Left (ToClient.DecodeError . pack) $ eitherDecodeStrict $ encodeUtf8 es
    cmsg <- ExceptT $ return $ over _Left (ToClient.DecodeError . pack) $ eitherDecodeStrict $ encodeUtf8 ecmsg
    mgs <- case megs of
      Nothing -> ExceptT $ return $ Right Nothing
      (Just (FiatGameStateMsg egs)) -> ExceptT $ return $ over _Left (ToClient.DecodeError . pack) $ eitherDecodeStrict $ encodeUtf8 egs
    ExceptT $ boolToEither ToClient.Unauthorized <$> isCmdAuthorized s mgs p cmsg
    let p' = ToServer.player cmsg
    case ToServer.cmd cmsg of
      ToServer.StartGame                -> case mgs of
        (Just _) -> ExceptT $ return $ Left ToClient.GameAlreadyStarted
        Nothing  -> lift (initialGameState s) >>= \case
          Left err -> ExceptT $ return $ Left $ ToClient.FailedToInitialize err
          Right (_,gs :: GameState g mv) -> do
            cgs <- lift $ toClientGameState p s gs
            ExceptT $ return $ Right (s,Just cgs)
      ToServer.UpdateSettings s'         -> ExceptT $ return $ Right (s',Nothing)
      ToServer.MakeMove mv -> case megs of
        Nothing -> ExceptT $ return $ Left ToClient.GameIsNotStarted
        Just (FiatGameStateMsg egs) -> do
          (gs :: GameState g mv) <- ExceptT $ return $ over _Left (ToClient.DecodeError . pack) $ eitherDecodeStrict $ encodeUtf8 egs
          let isSystem = case p' of
                          System -> True
                          _      -> False
          ExceptT $ boolToEither ToClient.NotYourTurn . (isSystem ||) <$> isPlayersTurn s gs (p',mv)
          ExceptT $ boolToEither ToClient.InvalidMove <$> isMoveValid s gs (p',mv)
          gs' <- lift $ makeMove s gs (p',mv)
          cgs <- lift $ toClientGameState p s gs'
          return (s, Just cgs)

boolToEither :: a -> Bool -> Either a ()
boolToEither _ True  = Right ()
boolToEither a False = Left a
