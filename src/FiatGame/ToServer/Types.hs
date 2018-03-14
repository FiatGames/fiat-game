{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module FiatGame.ToServer.Types where

import           Data.Aeson              hiding (Error)
import           Data.Aeson.TH
import           FiatGame.GameState
import qualified FiatGame.ToClient.Types as ToClient
import           GHC.Generics

data Cmd s mv = StartGame | UpdateSettings s | MakeMove mv
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''Cmd)

data Msg s mv = Msg
  { player :: FiatPlayer
  , cmd    :: Cmd s mv
  } deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''Msg)

data MsgProcessed s g mv
  = Error ToClient.Error
  | MsgProcessed (SettingsAndState s g mv)
  deriving (Eq,Show,Generic)

fromEither :: Either ToClient.Error (s, Maybe (GameState g mv)) -> MsgProcessed s g mv
fromEither (Left err)      = Error err
fromEither (Right (s,mgs)) = MsgProcessed (SettingsAndState s mgs)

