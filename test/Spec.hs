{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString.Lazy (ByteString)
import           Data.Text            (Text)
import           FiatGame
import           Test.Hspec


data NoGame = A | B
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''NoGame)
data NoSettings = NoSettings
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''NoSettings)
data NoMoves = ToA | ToB
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''NoMoves)
instance FiatGame NoGame NoSettings NoMoves where
  makeMove (FiatGameState A s _) _ = FiatGameState B s []
  makeMove (FiatGameState B s _) _ = FiatGameState A s []
  isMoveValid (FiatGameState A _ _) (FiatMove _ ToB) = True
  isMoveValid (FiatGameState B _ _) (FiatMove _ ToA) = True
  isMoveValid _ _                                    = False
  isPlayersTurn (FiatGameState A _ _) (FiatMove (FiatPlayer 0) _)  = True
  isPlayersTurn (FiatGameState B _ _)  (FiatMove (FiatPlayer 1) _) = True
  isPlayersTurn _ _                                                = False
  initial s = FiatGameState A s []

type NoGameFiatGameState = Either FiatMoveError (FiatGameState NoGame NoSettings NoMoves)

initialState :: FiatGameStateMsg
initialState = FiatGameStateMsg $ encode (FiatGameState A NoSettings [] :: FiatGameState NoGame NoSettings NoMoves)
systemMove :: FiatMoveMsg
systemMove = FiatMoveMsg $ encode $ FiatMove System ToB
goodMove :: FiatMoveMsg
goodMove = FiatMoveMsg $ encode $ FiatMove (FiatPlayer 0) ToB
invalidMove :: FiatMoveMsg
invalidMove = FiatMoveMsg $ encode $ FiatMove (FiatPlayer 0) ToA
unauthorizedMove :: FiatMoveMsg
unauthorizedMove = FiatMoveMsg $ encode $ FiatMove (FiatPlayer 1) ToB

main :: IO ()
main = hspec $
  describe "processFromWebSocket" $ do
    it "good"
      $ (processFromWebSocket (FiatPlayer 0) initialState goodMove :: NoGameFiatGameState) `shouldBe` Right (FiatGameState B NoSettings [])
    it "system allowed"
      $ (processFromWebSocket System initialState systemMove :: NoGameFiatGameState) `shouldBe` Right (FiatGameState B NoSettings [])
    it "system allowed to move on other's behalf"
      $ (processFromWebSocket System initialState goodMove :: NoGameFiatGameState) `shouldBe` Right (FiatGameState B NoSettings [])
    it "unauthorized"
      $ (processFromWebSocket (FiatPlayer 0) initialState unauthorizedMove :: NoGameFiatGameState) `shouldBe` Left Unauthorized
    it "not your turn"
      $ (processFromWebSocket (FiatPlayer 1) initialState unauthorizedMove :: NoGameFiatGameState) `shouldBe` Left NotYourTurn
    it "invalid"
      $ (processFromWebSocket (FiatPlayer 0) initialState invalidMove :: NoGameFiatGameState) `shouldBe` Left Invalid
    it "decode error"
      $ (processFromWebSocket (FiatPlayer 1) (FiatGameStateMsg "") (FiatMoveMsg "") :: NoGameFiatGameState) `shouldBe` Left (DecodeError "Error in $: not enough input")

