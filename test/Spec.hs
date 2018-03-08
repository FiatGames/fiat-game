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
import           Data.ByteString.Lazy      (ByteString, toStrict)
import           Data.Text                 (Text)
import           Data.Text.Encoding
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
  addPlayer p (NoSettings ps)
    | length ps < 2 = return $ Just (NoSettings (p:ps))
    | otherwise = return Nothing
  makeMove s (FiatGameState A _) _ = return $ FiatGameState B []
  makeMove s (FiatGameState B _) _ = return $ FiatGameState A []
  isMoveValid _ (FiatGameState A _) (FiatMove _ ToB) = return True
  isMoveValid _ (FiatGameState B _) (FiatMove _ ToA) = return True
  isMoveValid _ _ _                                  = return False
  isPlayersTurn _ (FiatGameState A _) (FiatMove (FiatPlayer 0) _)  = return True
  isPlayersTurn _ (FiatGameState B _)  (FiatMove (FiatPlayer 1) _) = return True
  isPlayersTurn _ _ _                                                = return False
  initialGameState s = return $ Right (s,FiatGameState A [])

type NoGameFiatGameState = Identity (Either FiatMoveError (FiatGameState NoGame NoMoves))

initSettings :: NoSettings
initSettings = NoSettings []

goodSettings :: Maybe NoSettings
goodSettings = runIdentity $ do
  i <- initialSettings
  runMaybeT $ foldM (\s p -> MaybeT $ addPlayer p s) i [FiatPlayer 0, FiatPlayer 1]
badSettings :: Maybe NoSettings
badSettings = runIdentity $ do
    i <- initialSettings
    runMaybeT $ foldM (\s p -> MaybeT $ addPlayer p s) i [FiatPlayer 0, FiatPlayer 1, FiatPlayer 2]

initialState :: FiatGameStateMsg
initialState = FiatGameStateMsg $ decodeUtf8 $ toStrict $ encode (FiatGameState A [] :: FiatGameState NoGame NoMoves)
systemMove :: FiatMoveMsg
systemMove = FiatMoveMsg $ decodeUtf8 $ toStrict $ encode $ FiatMove System ToB
goodMove :: FiatMoveMsg
goodMove = FiatMoveMsg $ decodeUtf8 $ toStrict $ encode $ FiatMove (FiatPlayer 0) ToB
invalidMove :: FiatMoveMsg
invalidMove = FiatMoveMsg $ decodeUtf8 $ toStrict $ encode $ FiatMove (FiatPlayer 0) ToA
unauthorizedMove :: FiatMoveMsg
unauthorizedMove = FiatMoveMsg $ decodeUtf8 $ toStrict $ encode $ FiatMove (FiatPlayer 1) ToB

main :: IO ()
main = hspec $ do
  describe "processFromWebSocket" $ do
    it "good"
      $ runIdentity (processFromWebSocket (FiatPlayer 0) initSettings initialState goodMove :: NoGameFiatGameState) `shouldBe` Right (FiatGameState B [])
    it "system allowed"
      $ runIdentity (processFromWebSocket System initSettings initialState systemMove :: NoGameFiatGameState) `shouldBe` Right (FiatGameState B [])
    it "system allowed to move on other's behalf"
      $ runIdentity (processFromWebSocket System initSettings initialState goodMove :: NoGameFiatGameState) `shouldBe` Right (FiatGameState B [])
    it "unauthorized"
      $ runIdentity (processFromWebSocket (FiatPlayer 0) initSettings initialState unauthorizedMove :: NoGameFiatGameState) `shouldBe` Left Unauthorized
    it "not your turn"
      $ runIdentity (processFromWebSocket (FiatPlayer 1) initSettings initialState unauthorizedMove :: NoGameFiatGameState) `shouldBe` Left NotYourTurn
    it "invalid"
      $ runIdentity (processFromWebSocket (FiatPlayer 0) initSettings initialState invalidMove :: NoGameFiatGameState) `shouldBe` Left Invalid
    it "decode error"
      $ runIdentity (processFromWebSocket (FiatPlayer 1) initSettings (FiatGameStateMsg "") (FiatMoveMsg "") :: NoGameFiatGameState) `shouldBe` Left (DecodeError "Error in $: not enough input")
  describe "addPlayer" $ do
    it "good"
      $ goodSettings `shouldBe` Just (NoSettings [FiatPlayer 1, FiatPlayer 0])
    it "game is full"
      $ badSettings `shouldBe` Nothing
