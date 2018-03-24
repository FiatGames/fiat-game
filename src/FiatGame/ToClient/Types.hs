{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module FiatGame.ToClient.Types where

import           Data.Aeson     hiding (Error)
import           Data.Aeson.TH
import           Data.Text      (Text)
import           FiatGame.Types
import           GHC.Generics

data Error = GameIsNotStarted
           | GameAlreadyStarted
           | InvalidMove
           | Unauthorized
           | NotYourTurn
           | NotEnoughPlayers
           | DecodeError Text
           | FailedToInitialize Text
           | GameStateOutOfDate
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''Error)

data Msg s g mv
  = Error FiatPlayer Error
  | Msg FiatGameHash s (Maybe (GameState g mv))
  deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''Msg)

fromMsg :: Msg s g mv -> Either (FiatPlayer,Error) (FiatGameHash, s, Maybe (GameState g mv))
fromMsg (Error p e)  = Left (p,e)
fromMsg (Msg h s gs) = Right (h,s,gs)

toMsg :: Either (FiatPlayer,Error) (FiatGameHash, s, Maybe (GameState g mv)) -> Msg s g mv
toMsg (Left (p,e))     = Error p e
toMsg (Right (h,s,gs)) = Msg h s gs
