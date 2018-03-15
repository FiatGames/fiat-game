{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module FiatGame.ToServer.Types where

import           Data.Aeson         hiding (Error)
import           Data.Aeson.TH
import           FiatGame.GameState
import           GHC.Generics

data Cmd s mv = StartGame | UpdateSettings s | MakeMove mv
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''Cmd)

data Msg s mv = Msg
  { player :: Player
  , cmd    :: Cmd s mv
  } deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''Msg)
