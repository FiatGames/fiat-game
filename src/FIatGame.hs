{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module FiatGame where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString.Lazy (ByteString)
import           Data.Int
import           Data.Text            (Text)
import           GHC.Generics

data FiatPlayer = FiatPlayer Int64
  deriving (Eq,Ord,Show,Generic)
$(deriveJSON defaultOptions ''FiatPlayer)
data FiatMove m = FiatMove FiatPlayer m
  deriving (Eq,Ord,Show,Generic,Functor)
$(deriveJSON defaultOptions ''FiatMove)

class (ToJSON gDTO, FromJSON gDTO, ToJSON m, FromJSON m) => FiatGame g gDTO m | g -> m, g -> gDTO where
  initialState :: g
  makeMove :: g -> FiatMove m -> Either Text g
  playerAllowed :: g -> FiatPlayer -> FiatMove m -> Bool
  gameStateIso :: Iso' g gDTO

note :: Text -> Maybe a -> Either Text a
note t = maybe (Left t) Right

makeMoveIfAllowed :: (FiatGame g gDTO m) => g -> FiatMove m -> Either Text g
makeMoveIfAllowed gs mv@(FiatMove p _) = if playerAllowed gs p mv
                                         then makeMove gs mv
                                         else Left "That player is not allowed to make that move"

playGame :: (FiatGame g gDTO m) => [ByteString] -> Either Text g
playGame mvs = note "Couldn't decode moves" (mapM decode mvs) >>= foldM makeMoveIfAllowed initialState
