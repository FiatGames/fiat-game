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
  { players  :: [FiatPlayer]
  , changeMe :: Bool
  }
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''NoSettings)
data NoMoves = ToA | ToB
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''NoMoves)

instance FiatGame Identity NoGame NoSettings NoMoves where
  initialSettings = return initSettings
  addPlayer p (NoSettings ps c)
    | length ps < 2 = return $ Just (NoSettings (p:ps) c)
    | otherwise = return Nothing
  makeMove s (FiatGameState A _) _ = return $ FiatGameState B []
  makeMove s (FiatGameState B _) _ = return $ FiatGameState A []
  isMoveValid _ (FiatGameState A _) (_, ToB) = return True
  isMoveValid _ (FiatGameState B _) (_, ToA) = return True
  isMoveValid _ _ _                          = return False
  isPlayersTurn _ (FiatGameState A _) (FiatPlayer 0, _)  = return True
  isPlayersTurn _ (FiatGameState B _)  (FiatPlayer 1, _) = return True
  isPlayersTurn _ _ _                                    = return False
  initialGameState s
    | length (players s) < 2 = return $ Left "Not enough players"
    | otherwise = return $ Right (s,FiatGameState A [])

type NoGameFiatGameState = Identity (FiatToClient NoSettings NoGame NoMoves)

initSettings :: NoSettings
initSettings = NoSettings [] False
initSettingsMsg :: FiatGameSettingsMsg
initSettingsMsg = FiatGameSettingsMsg $ decodeUtf8 $ toStrict $ encode initSettings
changedSettings :: NoSettings
changedSettings = NoSettings [] True
twoPlayersSettings :: NoSettings
twoPlayersSettings = NoSettings [FiatPlayer 0, FiatPlayer 1] False
twoPlayerSettingsMsg :: FiatGameSettingsMsg
twoPlayerSettingsMsg = FiatGameSettingsMsg $ decodeUtf8 $ toStrict $ encode twoPlayersSettings

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
systemMove :: FiatFromClientMsg
systemMove = FiatFromClientMsg $ decodeUtf8 $ toStrict $ encode (FiatFromClient System (MakeMove ToB) :: FiatFromClient NoSettings NoMoves)
goodMove :: FiatFromClientMsg
goodMove = FiatFromClientMsg $ decodeUtf8 $ toStrict $ encode (FiatFromClient (FiatPlayer 0) (MakeMove ToB) :: FiatFromClient NoSettings NoMoves)
invalidMove :: FiatFromClientMsg
invalidMove = FiatFromClientMsg $ decodeUtf8 $ toStrict $ encode (FiatFromClient (FiatPlayer 0) (MakeMove ToA)  :: FiatFromClient NoSettings NoMoves)
unauthorizedMove :: FiatFromClientMsg
unauthorizedMove = FiatFromClientMsg $ decodeUtf8 $ toStrict $ encode (FiatFromClient (FiatPlayer 1) (MakeMove ToB)  :: FiatFromClient NoSettings NoMoves)
startGame :: FiatFromClientMsg
startGame =  FiatFromClientMsg $ decodeUtf8 $ toStrict $ encode (FiatFromClient System StartGame :: FiatFromClient NoSettings NoMoves)
updateSettings :: FiatFromClientMsg
updateSettings =  FiatFromClientMsg $ decodeUtf8 $ toStrict $ encode (FiatFromClient System (UpdateSettings changedSettings) :: FiatFromClient NoSettings NoMoves)
main :: IO ()
main = hspec $ do
  describe "processFromWebSocket" $ do
    it "good"
      $ runIdentity (processFromWebSocket (FiatPlayer 0) initSettingsMsg (Just initialState) goodMove :: NoGameFiatGameState) `shouldBe` FiatToClient initSettings (Just $ FiatGameState B [])
    it "start game"
      $ runIdentity (processFromWebSocket System twoPlayerSettingsMsg Nothing startGame :: NoGameFiatGameState) `shouldBe` FiatToClient twoPlayersSettings (Just $ FiatGameState A [])
    it "update settings"
      $ runIdentity (processFromWebSocket System initSettingsMsg Nothing updateSettings :: NoGameFiatGameState) `shouldBe` FiatToClient changedSettings Nothing
    it "failed to start game"
      $ runIdentity (processFromWebSocket System initSettingsMsg Nothing startGame :: NoGameFiatGameState) `shouldBe` FromClientError (FailedToInitialize "Not enough players")
    it "system allowed"
      $ runIdentity (processFromWebSocket System initSettingsMsg (Just initialState) systemMove :: NoGameFiatGameState) `shouldBe` FiatToClient initSettings (Just $ FiatGameState B [])
    it "system allowed to move on other's behalf"
      $ runIdentity (processFromWebSocket System initSettingsMsg (Just initialState) goodMove :: NoGameFiatGameState) `shouldBe` FiatToClient initSettings (Just$ FiatGameState B [])
    it "unauthorized"
      $ runIdentity (processFromWebSocket (FiatPlayer 0) initSettingsMsg (Just initialState) unauthorizedMove :: NoGameFiatGameState) `shouldBe` FromClientError Unauthorized
    it "not your turn"
      $ runIdentity (processFromWebSocket (FiatPlayer 1) initSettingsMsg (Just initialState) unauthorizedMove :: NoGameFiatGameState) `shouldBe` FromClientError NotYourTurn
    it "invalid"
      $ runIdentity (processFromWebSocket (FiatPlayer 0) initSettingsMsg (Just initialState) invalidMove :: NoGameFiatGameState) `shouldBe` FromClientError InvalidMove
    it "game is not started"
      $ runIdentity (processFromWebSocket (FiatPlayer 0) initSettingsMsg Nothing invalidMove :: NoGameFiatGameState) `shouldBe` FromClientError GameIsNotStarted
    it "game already started"
      $ runIdentity (processFromWebSocket System initSettingsMsg (Just initialState) startGame :: NoGameFiatGameState) `shouldBe` FromClientError GameAlreadyStarted
    it "decode error"
      $ runIdentity (processFromWebSocket (FiatPlayer 1) initSettingsMsg (Just $ FiatGameStateMsg "") (FiatFromClientMsg "") :: NoGameFiatGameState) `shouldBe` FromClientError (DecodeError "Error in $: not enough input")
  describe "addPlayer" $ do
    it "good"
      $ goodSettings `shouldBe` Just (NoSettings [FiatPlayer 1, FiatPlayer 0] False)
    it "game is full"
      $ badSettings `shouldBe` Nothing
