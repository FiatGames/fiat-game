{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module FiatGame where

import           Control.Lens
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString.Lazy (ByteString)
import           Data.Int
import           Data.Text            (Text, pack)
import           Data.Text.Encoding
import           Data.Time.Clock
import           GHC.Generics

data FiatPlayer = FiatPlayer Int64 | System
  deriving (Eq,Ord,Show,Generic)
$(deriveJSON defaultOptions ''FiatPlayer)

data FutureMove m = FutureMove UTCTime m
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''FutureMove)

data FiatGameState g m = FiatGameState g [FutureMove m]
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''FiatGameState)

data FiatFromClientError = GameIsNotStarted | GameAlreadyStarted | InvalidMove | Unauthorized | NotYourTurn | NotEnoughPlayers | DecodeError Text | FailedToInitialize Text
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''FiatFromClientError)

data FiatFromClientCmd s mv = StartGame | UpdateSettings s | MakeMove mv
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''FiatFromClientCmd)

data FiatToClient s g mv
  = FromClientError FiatFromClientError
  | FiatToClient
    { fiatToClientSettings :: s
    , fiatToClientState    :: Maybe (FiatGameState g mv)
    }
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''FiatToClient)

fromEither (Left err)      = FromClientError err
fromEither (Right (s,mgs)) = FiatToClient s mgs

data FiatFromClient s mv = FiatFromClient
  { fiatFromClientPlayer :: FiatPlayer
  , fiatFromClientCmd    :: FiatFromClientCmd s mv
  } deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''FiatFromClient)

newtype FiatGameSettingsMsg = FiatGameSettingsMsg {getGameSettingsMsg :: Text}
  deriving (Eq,Show,Generic)

newtype FiatGameStateMsg = FiatGameStateMsg {getGameStateMsg :: Text}
  deriving (Eq,Show,Generic)

newtype FiatFromClientMsg = FiatFromClientMsg {getFromClientMsg :: Text}
  deriving (Eq,Show,Generic)

class (Monad m, ToJSON mv, FromJSON mv, ToJSON g, FromJSON g, ToJSON s, FromJSON s) => FiatGame m g s mv | s -> mv, s -> g where
  initialSettings :: m s
  addPlayer :: FiatPlayer -> s -> m (Maybe s)
  initialGameState :: s -> m (Either Text (s,FiatGameState g mv))
  makeMove :: s -> FiatGameState g mv -> (FiatPlayer, mv) -> m (FiatGameState g mv)
  isPlayersTurn :: s -> FiatGameState g mv -> (FiatPlayer, mv) -> m Bool
  isMoveValid :: s -> FiatGameState g mv -> (FiatPlayer, mv) -> m Bool
  isCmdAuthorized :: s -> Maybe (FiatGameState g mv) -> FiatPlayer -> FiatFromClient s mv -> m Bool
  isCmdAuthorized _ _ System _ = return True
  isCmdAuthorized _ _ (FiatPlayer p1) fc = case fiatFromClientPlayer fc of
    System          -> return False
    (FiatPlayer p2) -> return $ p1 == p2
  processFromWebSocket :: FiatPlayer -> FiatGameSettingsMsg -> Maybe FiatGameStateMsg -> FiatFromClientMsg -> m (FiatToClient s g mv)
  processFromWebSocket p (FiatGameSettingsMsg es) megs (FiatFromClientMsg ecmsg) = fmap fromEither $ runExceptT $ do
    s <- ExceptT $ return $ over _Left (DecodeError . pack) $ eitherDecodeStrict $ encodeUtf8 es
    cmsg <- ExceptT $ return $ over _Left (DecodeError . pack) $ eitherDecodeStrict $ encodeUtf8 ecmsg
    mgs <- case megs of
      Nothing -> ExceptT $ return $ Right Nothing
      (Just (FiatGameStateMsg egs)) -> ExceptT $ return $ over _Left (DecodeError . pack) $ eitherDecodeStrict $ encodeUtf8 egs
    ExceptT $ boolToEither Unauthorized <$> isCmdAuthorized s mgs p cmsg
    let p' = fiatFromClientPlayer cmsg
    case fiatFromClientCmd cmsg of
      StartGame                -> case mgs of
        (Just _) -> ExceptT $ return $ Left GameAlreadyStarted
        Nothing  -> do
          egs <- lift $ initialGameState s
          ExceptT $ return
                  $ over _Left FailedToInitialize
                  $ over (_Right._2) Just egs
      UpdateSettings s'         -> ExceptT $ return $ Right (s',Nothing)
      MakeMove mv -> case megs of
        Nothing -> ExceptT $ return $ Left GameIsNotStarted
        Just (FiatGameStateMsg egs) -> do
          gs <- ExceptT $ return $ over _Left (DecodeError . pack) $ eitherDecodeStrict $ encodeUtf8 egs
          let isSystem = case p' of
                          System -> True
                          _      -> False
          ExceptT $ boolToEither NotYourTurn . (isSystem ||) <$> isPlayersTurn s gs (p',mv)
          ExceptT $ boolToEither InvalidMove <$> isMoveValid s gs (p',mv)
          lift $ (,) s . Just <$> makeMove s gs (p',mv)

boolToEither :: a -> Bool -> Either a ()
boolToEither _ True  = Right ()
boolToEither a False = Left a
