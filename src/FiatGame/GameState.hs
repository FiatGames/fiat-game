{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module FiatGame.GameState where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Time.Clock
import           GHC.Generics
import           GHC.Int

data FiatPlayer = FiatPlayer Int64 | System
  deriving (Eq,Ord,Show,Generic)
$(deriveJSON defaultOptions ''FiatPlayer)

data FutureMove m = FutureMove UTCTime m
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''FutureMove)

data GameState g m = GameState g [FutureMove m]
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''GameState)
