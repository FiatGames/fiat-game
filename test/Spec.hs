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
import           FiatGame.Class
import           FiatGame.GameState
import qualified FiatGame.ToClient.Types   as ToClient
import qualified FiatGame.ToServer.Types   as ToServer
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

instance FiatGame Identity NoGame NoSettings NoMoves NoGame where
  defaultSettings = return initSettings
  addPlayer p (NoSettings ps c)
    | length ps < 2 = return $ Just (NoSettings (p:ps) c)
    | otherwise = return Nothing
  makeMove _ (GameState stage A _) _ = return $ GameState stage B Nothing
  makeMove _ (GameState stage B _) _ = return $ GameState stage A Nothing
  isMoveValid _ (GameState _ A _) (_, ToB) = return True
  isMoveValid _ (GameState _ B _) (_, ToA) = return True
  isMoveValid _ _ _                        = return False
  isPlayersTurn _ (GameState _ A _) (FiatPlayer 0, _)  = return True
  isPlayersTurn _ (GameState _ B _)  (FiatPlayer 1, _) = return True
  isPlayersTurn _ _ _                                  = return False
  initialGameState s
    | length (players s) < 2 = return $ Left "Not enough players"
    | otherwise = return $ Right (s,GameState Playing A Nothing)
  toClientGameState _ _ = return

type NoGameFiatGameState = Identity (ToClient.Msg NoSettings NoGame NoMoves)
type NoGameToServerMsg = ToServer.Msg NoSettings NoMoves

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
  i <- defaultSettings
  runMaybeT $ foldM (\s p -> MaybeT $ addPlayer p s) i [FiatPlayer 0, FiatPlayer 1]
badSettings :: Maybe NoSettings
badSettings = runIdentity $ do
    i <- defaultSettings
    runMaybeT $ foldM (\s p -> MaybeT $ addPlayer p s) i [FiatPlayer 0, FiatPlayer 1, FiatPlayer 2]

initialState :: FiatGameStateMsg
initialState = FiatGameStateMsg $ decodeUtf8 $ toStrict $ encode (GameState Playing A Nothing :: GameState NoGame NoMoves)
systemMove :: FiatToServertMsg
systemMove = FiatToServertMsg $ decodeUtf8 $ toStrict $ encode (ToServer.Msg System (ToServer.MakeMove ToB) :: NoGameToServerMsg)
goodMove :: FiatToServertMsg
goodMove = FiatToServertMsg $ decodeUtf8 $ toStrict $ encode (ToServer.Msg (FiatPlayer 0) (ToServer.MakeMove ToB) :: NoGameToServerMsg)
invalidMove :: FiatToServertMsg
invalidMove = FiatToServertMsg $ decodeUtf8 $ toStrict $ encode (ToServer.Msg (FiatPlayer 0) (ToServer.MakeMove ToA) :: NoGameToServerMsg)
unauthorizedMove :: FiatToServertMsg
unauthorizedMove = FiatToServertMsg $ decodeUtf8 $ toStrict $ encode (ToServer.Msg (FiatPlayer 1) (ToServer.MakeMove ToB) :: NoGameToServerMsg)
startGame :: FiatToServertMsg
startGame =  FiatToServertMsg $ decodeUtf8 $ toStrict $ encode (ToServer.Msg System ToServer.StartGame :: NoGameToServerMsg)
updateSettings :: FiatToServertMsg
updateSettings =  FiatToServertMsg $ decodeUtf8 $ toStrict $ encode (ToServer.Msg System (ToServer.UpdateSettings changedSettings) :: NoGameToServerMsg)
main :: IO ()
main = hspec $ do
  describe "processToServer" $ do
    it "good"
      $ runIdentity (processToServer (FiatPlayer 0) initSettingsMsg (Just initialState) goodMove :: NoGameFiatGameState) `shouldBe` ToClient.Msg initSettings (Just $ GameState Playing B Nothing)
    it "start game"
      $ runIdentity (processToServer System twoPlayerSettingsMsg Nothing startGame :: NoGameFiatGameState) `shouldBe` ToClient.Msg twoPlayersSettings (Just $ GameState Playing A Nothing)
    it "update settings"
      $ runIdentity (processToServer System initSettingsMsg Nothing updateSettings :: NoGameFiatGameState) `shouldBe` ToClient.Msg changedSettings Nothing
    it "failed to start game"
      $ runIdentity (processToServer System initSettingsMsg Nothing startGame :: NoGameFiatGameState) `shouldBe` ToClient.Error (ToClient.FailedToInitialize "Not enough players")
    it "system allowed"
      $ runIdentity (processToServer System initSettingsMsg (Just initialState) systemMove :: NoGameFiatGameState) `shouldBe` ToClient.Msg initSettings (Just $ GameState Playing B Nothing)
    it "system allowed to move on other's behalf"
      $ runIdentity (processToServer System initSettingsMsg (Just initialState) goodMove :: NoGameFiatGameState) `shouldBe` ToClient.Msg initSettings (Just$ GameState Playing B Nothing)
    it "unauthorized"
      $ runIdentity (processToServer (FiatPlayer 0) initSettingsMsg (Just initialState) unauthorizedMove :: NoGameFiatGameState) `shouldBe` ToClient.Error ToClient.Unauthorized
    it "not your turn"
      $ runIdentity (processToServer (FiatPlayer 1) initSettingsMsg (Just initialState) unauthorizedMove :: NoGameFiatGameState) `shouldBe` ToClient.Error ToClient.NotYourTurn
    it "invalid"
      $ runIdentity (processToServer (FiatPlayer 0) initSettingsMsg (Just initialState) invalidMove :: NoGameFiatGameState) `shouldBe` ToClient.Error ToClient.InvalidMove
    it "game is not started"
      $ runIdentity (processToServer (FiatPlayer 0) initSettingsMsg Nothing invalidMove :: NoGameFiatGameState) `shouldBe` ToClient.Error ToClient.GameIsNotStarted
    it "game already started"
      $ runIdentity (processToServer System initSettingsMsg (Just initialState) startGame :: NoGameFiatGameState) `shouldBe` ToClient.Error ToClient.GameAlreadyStarted
    it "decode error"
      $ runIdentity (processToServer (FiatPlayer 1) initSettingsMsg (Just $ FiatGameStateMsg "") (FiatToServertMsg "") :: NoGameFiatGameState) `shouldBe` ToClient.Error (ToClient.DecodeError "Error in $: not enough input")
  describe "addPlayer" $ do
    it "good"
      $ goodSettings `shouldBe` Just (NoSettings [FiatPlayer 1, FiatPlayer 0] False)
    it "game is full"
      $ badSettings `shouldBe` Nothing
