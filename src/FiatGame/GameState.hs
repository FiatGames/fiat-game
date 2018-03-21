{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module FiatGame.GameState where

import           Control.Lens.TH
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text       (Text)
import           Data.Time.Clock
import           GHC.Generics
import           GHC.Int

data FiatPlayer = FiatPlayer Int64 | System
  deriving (Eq,Ord,Show,Generic)
$(deriveJSON defaultOptions ''FiatPlayer)

newtype FiatGameHash = FiatGameHash Text
  deriving (Eq,Ord,Show,Generic)
$(deriveJSON defaultOptions ''FiatGameHash)
makeWrapped ''FiatGameHash

data FutureMove m = FutureMove
  { _futureMoveHash :: FiatGameHash
  , _futureMoveTime :: UTCTime
  , _futureMoveMove :: m
  }
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''FutureMove)
makeLenses ''FutureMove

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
