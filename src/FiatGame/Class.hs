{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module FiatGame.Class where

import           Control.Lens
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString.Lazy    (ByteString)
import           Data.Int
import           Data.Text               (Text, pack)
import           Data.Text.Encoding
import           Data.Time.Clock
import           FiatGame.GameState
import qualified FiatGame.ToClient.Types as ToClient
import qualified FiatGame.ToServer.Types as ToServer
import           GHC.Generics

newtype FiatGameSettingsMsg = FiatGameSettingsMsg {getGameSettingsMsg :: Text}
  deriving (Eq,Show,Generic)

newtype FiatGameStateMsg = FiatGameStateMsg {getGameStateMsg :: Text}
  deriving (Eq,Show,Generic)

newtype FiatFromClientMsg = FiatFromClientMsg {getFromClientMsg :: Text}
  deriving (Eq,Show,Generic)

class (Monad m, ToJSON mv, FromJSON mv, ToJSON g, FromJSON g, ToJSON s, FromJSON s) => FiatGame m g s mv | s -> mv, s -> g where
  initialSettings :: m s
  addPlayer :: FiatPlayer -> s -> m (Maybe s)
  initialGameState :: s -> m (Either Text (s,GameState g mv))
  makeMove :: s -> GameState g mv -> (FiatPlayer, mv) -> m (GameState g mv)
  isPlayersTurn :: s -> GameState g mv -> (FiatPlayer, mv) -> m Bool
  isMoveValid :: s -> GameState g mv -> (FiatPlayer, mv) -> m Bool
  isCmdAuthorized :: s -> Maybe (GameState g mv) -> FiatPlayer -> ToServer.Msg s mv -> m Bool
  isCmdAuthorized _ _ System _ = return True
  isCmdAuthorized _ _ (FiatPlayer p1) fc = case ToServer.player fc of
    System          -> return False
    (FiatPlayer p2) -> return $ p1 == p2
  processFromWebSocket :: FiatPlayer -> FiatGameSettingsMsg -> Maybe FiatGameStateMsg -> FiatFromClientMsg -> m (ToClient.Msg s g mv)
  processFromWebSocket p (FiatGameSettingsMsg es) megs (FiatFromClientMsg ecmsg) = fmap ToClient.fromEither $ runExceptT $ do
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
        Nothing  -> do
          egs <- lift $ initialGameState s
          ExceptT $ return
                  $ over _Left ToClient.FailedToInitialize
                  $ over (_Right._2) Just egs
      ToServer.UpdateSettings s'         -> ExceptT $ return $ Right (s',Nothing)
      ToServer.MakeMove mv -> case megs of
        Nothing -> ExceptT $ return $ Left ToClient.GameIsNotStarted
        Just (FiatGameStateMsg egs) -> do
          gs <- ExceptT $ return $ over _Left (ToClient.DecodeError . pack) $ eitherDecodeStrict $ encodeUtf8 egs
          let isSystem = case p' of
                          System -> True
                          _      -> False
          ExceptT $ boolToEither ToClient.NotYourTurn . (isSystem ||) <$> isPlayersTurn s gs (p',mv)
          ExceptT $ boolToEither ToClient.InvalidMove <$> isMoveValid s gs (p',mv)
          lift $ (,) s . Just <$> makeMove s gs (p',mv)

boolToEither :: a -> Bool -> Either a ()
boolToEither _ True  = Right ()
boolToEither a False = Left a
