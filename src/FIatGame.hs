{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module FiatGame where

import           Control.Lens
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

data FiatGameState g m = FiatGameState g [FutureMove m]
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''FiatGameState)

data FiatMoveError = Unauthorized | NotYourTurn | DecodeError Text
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''FiatMoveError)

newtype FiatGameStateMsg = FiatGameStateMsg ByteString
  deriving (Eq,Show,Generic)

newtype FiatMoveMsg = FiatMoveMsg ByteString
  deriving (Eq,Show,Generic)

class (ToJSON m, FromJSON m, ToJSON g, FromJSON g) => FiatGame g m | g -> m where
  makeMove :: FiatGameState g m -> FiatMove m -> Either FiatMoveError (FiatGameState g m)
  isPlayersTurn :: FiatGameState g m -> FiatMove m -> Bool
  moveAuthorized :: FiatGameState g m -> FiatPlayer -> FiatMove m -> Bool
  moveAuthorized _ System _                                     = True
  moveAuthorized _ (FiatPlayer _) (FiatMove System _)           = False
  moveAuthorized _ (FiatPlayer p1) (FiatMove (FiatPlayer p2) _) = p1 == p2
  processFromWebSocket :: FiatPlayer -> FiatGameStateMsg -> FiatMoveMsg -> Either FiatMoveError (FiatGameState g m)
  processFromWebSocket p (FiatGameStateMsg egs) (FiatMoveMsg emv) = do
    gs <- over _Left (DecodeError . pack) $ eitherDecode egs
    mv <- over _Left (DecodeError . pack) $ eitherDecode emv
    boolToEither Unauthorized $ moveAuthorized gs p mv
    let isSystem = case mv of
                    (FiatMove System _) -> True
                    _                   -> False
    boolToEither NotYourTurn $ isSystem || isPlayersTurn gs mv
    makeMove gs mv

boolToEither :: a -> Bool -> Either a ()
boolToEither _ True  = Right ()
boolToEither a False = Left a
