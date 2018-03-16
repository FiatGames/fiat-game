{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module FiatGame.ToClient.Types where

import           Data.Aeson         hiding (Error)
import           Data.Aeson.TH
import           Data.Text          (Text)
import           FiatGame.GameState
import           GHC.Generics

data Error = GameIsNotStarted | GameAlreadyStarted | InvalidMove | Unauthorized | NotYourTurn | NotEnoughPlayers | DecodeError Text | FailedToInitialize Text | GameStateOutOfDate
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''Error)

data Msg s g mv
  = Error FiatPlayer Error
  | Msg (SettingsAndState s g mv)
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''Msg)
