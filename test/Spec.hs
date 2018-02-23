{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString.Lazy (ByteString)
import           Data.Text            (Text)
import           FiatGame
import           Test.Hspec

data NoGame = A | B
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''NoGame)
instance FiatGame NoGame NoGame () where
  initialState = A
  makeMove A _ = Right B
  makeMove B _ = Right A
  playerAllowed :: NoGame -> FiatPlayer -> FiatMove () -> Bool
  playerAllowed A _ (FiatMove (FiatPlayer 0) _) = True
  playerAllowed B _ (FiatMove (FiatPlayer 1) _) = True
  playerAllowed _ _ _                           = False
  gameStateIso = iso id id

badTurnOrder :: [ByteString]
badTurnOrder = map encode [FiatMove (FiatPlayer 0) (), FiatMove (FiatPlayer 0) ()]

goodGame :: [ByteString]
goodGame = map encode [FiatMove (FiatPlayer 0) (), FiatMove (FiatPlayer 1) ()]

main :: IO ()
main = hspec $
  describe "FiatGame" $ do
    it "playGame works"
      $ encode (view gameStateIso <$> (playGame goodGame :: Either Text NoGame)) `shouldBe` "{\"Right\":\"A\"}"
    it "playerAllowed works"
      $ encode (view gameStateIso <$> (playGame badTurnOrder :: Either Text NoGame)) `shouldBe` "{\"Left\":\"That player is not allowed to make that move\"}"
