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

data GameStage = SettingUp | Playing | Done
  deriving (Show, Read, Eq, Generic)
$(deriveJSON defaultOptions ''GameStage)

data GameState g m = GameState GameStage g (Maybe (FutureMove m))
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''GameState)

futureMove :: GameState g m -> Maybe (FutureMove m)
futureMove (GameState _ _ m) = m

data SettingsAndState s g m = SettingsAndState s (Maybe (GameState g m))
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''SettingsAndState)
