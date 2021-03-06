{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module FiatGame.ToServer.Types where

import           Data.Aeson     hiding (Error)
import           Data.Aeson.TH
import           FiatGame.Types
import           GHC.Generics

data Cmd s mv = StartGame | UpdateSettings s | MakeMove mv
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''Cmd)

data Msg s mv = Msg
  { player :: FiatPlayer
  , cmd    :: Cmd s mv
  , hash   :: FiatGameHash
  } deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''Msg)
