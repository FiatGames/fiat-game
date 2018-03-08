{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
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

data FiatMove m = FiatMove FiatPlayer m
  deriving (Eq,Ord,Show,Generic,Functor)
$(deriveJSON defaultOptions ''FiatMove)

data FutureMove m = FutureMove UTCTime (FiatMove m)
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''FutureMove)

data FiatGameState g m = FiatGameState g [FutureMove m]
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''FiatGameState)

data FiatMoveError = Invalid | Unauthorized | NotYourTurn | DecodeError Text
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''FiatMoveError)

newtype FiatGameStateMsg = FiatGameStateMsg Text
  deriving (Eq,Show,Generic)

newtype FiatMoveMsg = FiatMoveMsg Text
  deriving (Eq,Show,Generic)

class (Monad m, ToJSON mv, FromJSON mv, ToJSON g, FromJSON g, ToJSON s, FromJSON s) => FiatGame m g s mv | s -> mv, s -> g where
  initialSettings :: m s
  addPlayer :: FiatPlayer -> s -> m (Maybe s)
  initialGameState :: s -> m (Either Text (FiatGameState g mv))
  makeMove :: s -> FiatGameState g mv -> FiatMove mv -> m (FiatGameState g mv)
  isPlayersTurn :: s -> FiatGameState g mv -> FiatMove mv -> m Bool
  isMoveValid :: s -> FiatGameState g mv -> FiatMove mv -> m Bool
  isMoveAuthorized :: s -> FiatGameState g mv -> FiatPlayer -> FiatMove mv -> m Bool
  isMoveAuthorized _ _ System _                                     = return True
  isMoveAuthorized _ _ (FiatPlayer _) (FiatMove System _)           = return False
  isMoveAuthorized _ _ (FiatPlayer p1) (FiatMove (FiatPlayer p2) _) = return $ p1 == p2
  processFromWebSocket :: FiatPlayer -> s -> FiatGameStateMsg -> FiatMoveMsg -> m (Either FiatMoveError (FiatGameState g mv))
  processFromWebSocket p s (FiatGameStateMsg egs) (FiatMoveMsg emv) = runExceptT $ do
    gs <- ExceptT $ return $ over _Left (DecodeError . pack) $ eitherDecodeStrict $ encodeUtf8 egs
    mv <- ExceptT $ return $ over _Left (DecodeError . pack) $ eitherDecodeStrict $ encodeUtf8 emv
    ExceptT $ boolToEither Unauthorized <$> isMoveAuthorized s gs p mv
    let isSystem = case mv of
                    (FiatMove System _) -> True
                    _                   -> False
    ExceptT $ boolToEither NotYourTurn . (isSystem ||) <$> isPlayersTurn s gs mv
    ExceptT $ boolToEither Invalid <$> isMoveValid s gs mv
    lift $ makeMove s gs mv

boolToEither :: a -> Bool -> Either a ()
boolToEither _ True  = Right ()
boolToEither a False = Left a
