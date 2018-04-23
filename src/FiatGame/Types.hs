{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module FiatGame.Types where

import           Control.Lens.TH
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char       (toLower)
import           Data.Text       (Text)
import           Data.Time.Clock
import           GHC.Generics
import           GHC.Int

data FiatPlayer = FiatPlayer Int64 | System
  deriving (Eq,Ord,Show,Generic)
$(deriveJSON defaultOptions ''FiatPlayer)

newtype FiatGameHash = FiatGameHash Text
  deriving (Eq,Ord,Show,Generic,ToJSON,FromJSON)
makeWrapped ''FiatGameHash

data FutureMove m = FutureMove
  { _futureMoveTime :: UTCTime
  , _futureMoveMove :: m
  }
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''FutureMove)
makeLenses ''FutureMove

data GameStage = SettingUp | Playing | Done
  deriving (Show, Read, Eq, Generic)
$(deriveJSON defaultOptions ''GameStage)

data GameState g m = GameState
  { _gameStateStage      :: GameStage
  , _gameStateState      :: g
  , _gameStateFutureMove :: Maybe (FutureMove m)
  }
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''GameState)
makeLenses ''GameState

newtype SettingsMsg = SettingsMsg { getSettingsMsg :: Text }
  deriving (Eq,Show,Generic)
makeWrapped ''SettingsMsg

newtype GameStateMsg = GameStateMsg { getGameStateMsg :: Text }
  deriving (Eq,Show,Generic)
makeWrapped ''GameStateMsg

newtype ToServerMsg = ToServerMsg { getToServerMsg :: Text }
  deriving (Eq,Show,Generic)
makeWrapped ''ToServerMsg

newtype ToFiatMsg = ToFiatMsg { getToFiatMsg :: Text }
  deriving (Eq,Show,Generic)
makeWrapped ''ToFiatMsg

newtype ToClientMsg = ToClientMsg { getToClientMsg :: Text }
  deriving (Eq,Show,Generic)
makeWrapped ''ToClientMsg

newtype MoveSubmittedBy = MoveSubmittedBy { getSubmittedBy :: FiatPlayer }
  deriving (Eq,Show,Generic)
makeWrapped ''MoveSubmittedBy

data FromFiat = FromFiat
  { _fromFiatSettings  :: SettingsMsg
  , _fromFiatGameState :: Maybe GameStateMsg
  , _fromFiatGameHash  :: FiatGameHash
  } deriving (Eq,Show,Generic)
makeLenses ''FromFiat

data SuccessfulProcessed = SuccessfulProcessed
  { _successfulProcessedGameStage  :: GameStage
  , _successfulProccessedFromFiat  :: FromFiat
  , _successfulProcessedFutureMove :: Maybe (UTCTime, ToServerMsg)
  }
  deriving (Eq,Show,Generic)
makeLenses ''SuccessfulProcessed

data Processed = Processed
  { _processedToClientMsg        :: ToFiatMsg
  , _processedSuccessFul         :: Maybe SuccessfulProcessed
  , _processedGameStateOutOfDate :: Bool
  }
  deriving (Eq,Show,Generic)
makeLenses ''Processed
