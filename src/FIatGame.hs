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

data FiatGameState g s m = FiatGameState g s [FutureMove m]
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''FiatGameState)

data FiatMoveError = Invalid | Unauthorized | NotYourTurn | DecodeError Text
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''FiatMoveError)

newtype FiatGameStateMsg = FiatGameStateMsg ByteString
  deriving (Eq,Show,Generic)

newtype FiatMoveMsg = FiatMoveMsg ByteString
  deriving (Eq,Show,Generic)

class (Monad m, ToJSON mv, FromJSON mv, ToJSON g, FromJSON g, ToJSON s, FromJSON s) => FiatGame m g s mv | s -> mv, s -> g where
  initialSettings :: m s
  addPlayer :: s -> FiatPlayer -> m (Maybe s)
  initialGameState :: s -> m (FiatGameState g s mv)
  makeMove :: FiatGameState g s mv -> FiatMove mv -> m (FiatGameState g s mv)
  isPlayersTurn :: FiatGameState g s mv -> FiatMove mv -> m Bool
  isMoveValid :: FiatGameState g s mv -> FiatMove mv -> m Bool
  isMoveAuthorized :: FiatGameState g s mv -> FiatPlayer -> FiatMove mv -> m Bool
  isMoveAuthorized _ System _                                     = return True
  isMoveAuthorized _ (FiatPlayer _) (FiatMove System _)           = return False
  isMoveAuthorized _ (FiatPlayer p1) (FiatMove (FiatPlayer p2) _) = return $ p1 == p2
  processFromWebSocket :: FiatPlayer -> FiatGameStateMsg -> FiatMoveMsg -> m (Either FiatMoveError (FiatGameState g s mv))
  processFromWebSocket p (FiatGameStateMsg egs) (FiatMoveMsg emv) = runExceptT $ do
    gs <- ExceptT $ return $ over _Left (DecodeError . pack) $ eitherDecode egs
    mv <- ExceptT $ return $ over _Left (DecodeError . pack) $ eitherDecode emv
    ExceptT $ boolToEither Unauthorized <$> isMoveAuthorized gs p mv
    let isSystem = case mv of
                    (FiatMove System _) -> True
                    _                   -> False
    ExceptT $ boolToEither NotYourTurn . (isSystem ||) <$> isPlayersTurn gs mv
    ExceptT $ boolToEither Invalid <$> isMoveValid gs mv
    lift $ makeMove gs mv

boolToEither :: a -> Bool -> Either a ()
boolToEither _ True  = Right ()
boolToEither a False = Left a
