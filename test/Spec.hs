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
instance FiatGame NoGame () where
  makeMove (FiatGameState A _) _ = Right $ FiatGameState B []
  makeMove (FiatGameState B _) _ = Right $ FiatGameState A []
  isPlayersTurn (FiatGameState A _) (FiatMove (FiatPlayer 0) _)  = True
  isPlayersTurn (FiatGameState B _)  (FiatMove (FiatPlayer 1) _) = True
  isPlayersTurn _ _                                              = False

type NoGameFiatGameState = Either FiatMoveError (FiatGameState NoGame ())

initialState :: FiatGameStateMsg
initialState = FiatGameStateMsg $ encode (FiatGameState A [] :: FiatGameState NoGame ())
systemMove :: FiatMoveMsg
systemMove = FiatMoveMsg $ encode $ FiatMove System ()
goodMove :: FiatMoveMsg
goodMove = FiatMoveMsg $ encode $ FiatMove (FiatPlayer 0) ()
unauthorizedMove :: FiatMoveMsg
unauthorizedMove = FiatMoveMsg $ encode $ FiatMove (FiatPlayer 1) ()

main :: IO ()
main = hspec $
  describe "processFromWebSocket" $ do
    it "good"
      $ (processFromWebSocket (FiatPlayer 0) initialState goodMove :: NoGameFiatGameState) `shouldBe` Right (FiatGameState B [])
    it "system allowed"
      $ (processFromWebSocket System initialState systemMove :: NoGameFiatGameState) `shouldBe` Right (FiatGameState B [])
    it "system allowed to move on other's behalf"
      $ (processFromWebSocket System initialState goodMove :: NoGameFiatGameState) `shouldBe` Right (FiatGameState B [])
    it "unauthorized"
      $ (processFromWebSocket (FiatPlayer 0) initialState unauthorizedMove :: NoGameFiatGameState) `shouldBe` Left Unauthorized
    it "not your turn"
      $ (processFromWebSocket (FiatPlayer 1) initialState unauthorizedMove :: NoGameFiatGameState) `shouldBe` Left NotYourTurn
    it "decode error"
      $ (processFromWebSocket (FiatPlayer 1) (FiatGameStateMsg "") (FiatMoveMsg "") :: NoGameFiatGameState) `shouldBe` Left (DecodeError "Error in $: not enough input")

