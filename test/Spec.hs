{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString.Lazy      (ByteString)
import           Data.Text                 (Text)
import           FiatGame
import           Test.Hspec

data NoGame = A | B
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''NoGame)
data NoSettings = NoSettings
  { players :: [FiatPlayer]
  }
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''NoSettings)
data NoMoves = ToA | ToB
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''NoMoves)

instance FiatGame Identity NoGame NoSettings NoMoves where
  initialSettings = return initSettings
  addPlayer (NoSettings ps) p
    | length ps < 2 = return $ Just (NoSettings (p:ps))
    | otherwise = return Nothing
  makeMove (FiatGameState A s _) _ = return $ FiatGameState B s []
  makeMove (FiatGameState B s _) _ = return $ FiatGameState A s []
  isMoveValid (FiatGameState A _ _) (FiatMove _ ToB) = return True
  isMoveValid (FiatGameState B _ _) (FiatMove _ ToA) = return True
  isMoveValid _ _                                    = return False
  isPlayersTurn (FiatGameState A _ _) (FiatMove (FiatPlayer 0) _)  = return True
  isPlayersTurn (FiatGameState B _ _)  (FiatMove (FiatPlayer 1) _) = return True
  isPlayersTurn _ _                                                = return False
  initialGameState s = return $ FiatGameState A s []

type NoGameFiatGameState = Identity (Either FiatMoveError (FiatGameState NoGame NoSettings NoMoves))

initSettings :: NoSettings
initSettings = NoSettings []

goodSettings :: Maybe NoSettings
goodSettings = runIdentity $ do
  i <- initialSettings
  runMaybeT $ foldM (\s p -> MaybeT $ addPlayer s p) i [FiatPlayer 0, FiatPlayer 1]
badSettings :: Maybe NoSettings
badSettings = runIdentity $ do
    i <- initialSettings
    runMaybeT $ foldM (\s p -> MaybeT $ addPlayer s p) i [FiatPlayer 0, FiatPlayer 1, FiatPlayer 2]

initialState :: FiatGameStateMsg
initialState = FiatGameStateMsg $ encode (FiatGameState A initSettings [] :: FiatGameState NoGame NoSettings NoMoves)
systemMove :: FiatMoveMsg
systemMove = FiatMoveMsg $ encode $ FiatMove System ToB
goodMove :: FiatMoveMsg
goodMove = FiatMoveMsg $ encode $ FiatMove (FiatPlayer 0) ToB
invalidMove :: FiatMoveMsg
invalidMove = FiatMoveMsg $ encode $ FiatMove (FiatPlayer 0) ToA
unauthorizedMove :: FiatMoveMsg
unauthorizedMove = FiatMoveMsg $ encode $ FiatMove (FiatPlayer 1) ToB

main :: IO ()
main = hspec $ do
  describe "processFromWebSocket" $ do
    it "good"
      $ runIdentity (processFromWebSocket (FiatPlayer 0) initialState goodMove :: NoGameFiatGameState) `shouldBe` Right (FiatGameState B initSettings [])
    it "system allowed"
      $ runIdentity (processFromWebSocket System initialState systemMove :: NoGameFiatGameState) `shouldBe` Right (FiatGameState B initSettings [])
    it "system allowed to move on other's behalf"
      $ runIdentity (processFromWebSocket System initialState goodMove :: NoGameFiatGameState) `shouldBe` Right (FiatGameState B initSettings [])
    it "unauthorized"
      $ runIdentity (processFromWebSocket (FiatPlayer 0) initialState unauthorizedMove :: NoGameFiatGameState) `shouldBe` Left Unauthorized
    it "not your turn"
      $ runIdentity (processFromWebSocket (FiatPlayer 1) initialState unauthorizedMove :: NoGameFiatGameState) `shouldBe` Left NotYourTurn
    it "invalid"
      $ runIdentity (processFromWebSocket (FiatPlayer 0) initialState invalidMove :: NoGameFiatGameState) `shouldBe` Left Invalid
    it "decode error"
      $ runIdentity (processFromWebSocket (FiatPlayer 1) (FiatGameStateMsg "") (FiatMoveMsg "") :: NoGameFiatGameState) `shouldBe` Left (DecodeError "Error in $: not enough input")
  describe "addPlayer" $ do
    it "good"
      $ goodSettings `shouldBe` Just (NoSettings [FiatPlayer 1, FiatPlayer 0])
    it "game is full"
      $ badSettings `shouldBe` Nothing
