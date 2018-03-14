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

data NoGame = NoGame
  { turn     :: Bool
  , gsSecret :: ()
  }
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''NoGame)
data ClientNoGame = ClientNoGame
  { cTurn     :: Bool
  }
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''ClientNoGame)
data NoSettings = NoSettings
  { players              :: [FiatPlayer]
  , changeMe             :: Bool
  , itsASecretToEveryone :: ()
  }
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''NoSettings)
data ClientNoSettings = ClientNoSettings
  { csPlayers  :: [FiatPlayer]
  , csChangeMe :: Bool
  }
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''ClientNoSettings)
data NoMoves = ToA | ToB
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''NoMoves)

instance FiatGame Identity NoGame NoSettings NoMoves ClientNoGame ClientNoSettings where
  defaultSettings = return initSettings
  addPlayer p (NoSettings ps c ())
    | length ps < 2 = return $ Just (NoSettings (p:ps) c ())
    | otherwise = return Nothing
  makeMove _ _ (GameState stage (NoGame True _) _) _ = return $ GameState stage (NoGame False ()) Nothing
  makeMove _ _ (GameState stage (NoGame False _) _) _ = return $ GameState stage (NoGame True ()) Nothing
  isMoveValid _ _ (GameState _ (NoGame True _) _) ToB  = return True
  isMoveValid _ _ (GameState _ (NoGame False _) _) ToA = return True
  isMoveValid _ _ _ _                                  = return False
  isPlayersTurn (FiatPlayer 0) _ (GameState _ (NoGame True ()) _) _  = return True
  isPlayersTurn (FiatPlayer 1) _ (GameState _ (NoGame False _) _) _ = return True
  isPlayersTurn _ _ _ _                                  = return False
  initialGameState (NoSettings ps c ())
    | length ps < 2 = return $ Left "Not enough players"
    | otherwise = return $ Right (NoSettings ps (not c) (), GameState Playing (NoGame True ()) Nothing)
  toClientSettingsAndState _ (SettingsAndState (NoSettings ps c _) (Just (GameState stage (NoGame b ()) mvs))) = return (SettingsAndState (ClientNoSettings ps c) (Just (GameState stage (ClientNoGame b) mvs)))
  toClientSettingsAndState _ (SettingsAndState  (NoSettings ps c _) Nothing) = return (SettingsAndState (ClientNoSettings ps c) Nothing)

type NoGameFiatGameState = Identity (ToServer.MsgProcessed NoSettings NoGame NoMoves)
type NoGameClientMsg = Identity (ToClient.Msg ClientNoSettings ClientNoGame NoMoves)
type NoGameToServerMsg = ToServer.Msg NoSettings NoMoves

initSettings :: NoSettings
initSettings = NoSettings [] False ()
initClientSettings :: ClientNoSettings
initClientSettings = ClientNoSettings [] False
initSettingsMsg :: FiatGameSettingsMsg
initSettingsMsg = FiatGameSettingsMsg $ decodeUtf8 $ toStrict $ encode initSettings
changedSettings :: NoSettings
changedSettings = NoSettings [] True ()
twoPlayersSettings :: NoSettings
twoPlayersSettings = NoSettings [FiatPlayer 0, FiatPlayer 1] False ()
twoPlayerSettingsMsg :: FiatGameSettingsMsg
twoPlayerSettingsMsg = FiatGameSettingsMsg $ decodeUtf8 $ toStrict $ encode twoPlayersSettings
twoPlayersSettingsAfter :: NoSettings
twoPlayersSettingsAfter = NoSettings [FiatPlayer 0, FiatPlayer 1] True ()

goodSettings :: Maybe NoSettings
goodSettings = runIdentity $ do
  i <- defaultSettings
  runMaybeT $ foldM (\s p -> MaybeT $ addPlayer p s) i [FiatPlayer 0, FiatPlayer 1]
badSettings :: Maybe NoSettings
badSettings = runIdentity $ do
    i <- defaultSettings
    runMaybeT $ foldM (\s p -> MaybeT $ addPlayer p s) i [FiatPlayer 0, FiatPlayer 1, FiatPlayer 2]

initialState  :: GameState NoGame NoMoves
initialState = GameState Playing (NoGame True ()) Nothing
initialClientState  :: GameState ClientNoGame NoMoves
initialClientState = GameState Playing (ClientNoGame True) Nothing
initialStateMsg :: FiatGameStateMsg
initialStateMsg = FiatGameStateMsg $ decodeUtf8 $ toStrict $ encode initialState
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

goodProcessToServer :: NoGameFiatGameState
goodProcessToServer = processToServer (FiatMoveSubmittedBy (FiatPlayer 0)) initSettingsMsg (Just initialStateMsg) goodMove
goodToClientMsg :: NoGameClientMsg
goodToClientMsg = goodProcessToServer >>= toClientMsg (FiatPlayer 0) . Left

main :: IO ()
main = hspec $ do
  describe "processToServer" $ do
    it "good"
      $ runIdentity goodProcessToServer `shouldBe` ToServer.MsgProcessed (SettingsAndState initSettings (Just $ GameState Playing (NoGame False ()) Nothing))
    it "start game"
      $ runIdentity (processToServer (FiatMoveSubmittedBy System) twoPlayerSettingsMsg Nothing startGame :: NoGameFiatGameState) `shouldBe` ToServer.MsgProcessed (SettingsAndState twoPlayersSettingsAfter (Just $ GameState Playing (NoGame True ()) Nothing))
    it "update settings"
      $ runIdentity (processToServer (FiatMoveSubmittedBy System) initSettingsMsg Nothing updateSettings :: NoGameFiatGameState) `shouldBe` ToServer.MsgProcessed (SettingsAndState changedSettings Nothing)
    it "failed to start game"
      $ runIdentity (processToServer (FiatMoveSubmittedBy System) initSettingsMsg Nothing startGame :: NoGameFiatGameState) `shouldBe` ToServer.Error (ToClient.FailedToInitialize "Not enough players")
    it "system allowed"
      $ runIdentity (processToServer (FiatMoveSubmittedBy System) initSettingsMsg (Just initialStateMsg) systemMove :: NoGameFiatGameState) `shouldBe` ToServer.MsgProcessed (SettingsAndState initSettings (Just $ GameState Playing (NoGame False ()) Nothing))
    it "system allowed to move on other's behalf"
      $ runIdentity (processToServer (FiatMoveSubmittedBy System) initSettingsMsg (Just initialStateMsg) goodMove :: NoGameFiatGameState) `shouldBe` ToServer.MsgProcessed (SettingsAndState initSettings (Just $ GameState Playing (NoGame False ()) Nothing))
    it "unauthorized"
      $ runIdentity (processToServer (FiatMoveSubmittedBy (FiatPlayer 0)) initSettingsMsg (Just initialStateMsg) unauthorizedMove :: NoGameFiatGameState) `shouldBe` ToServer.Error ToClient.Unauthorized
    it "not your turn"
      $ runIdentity (processToServer (FiatMoveSubmittedBy (FiatPlayer 1)) initSettingsMsg (Just initialStateMsg) unauthorizedMove :: NoGameFiatGameState) `shouldBe` ToServer.Error ToClient.NotYourTurn
    it "invalid"
      $ runIdentity (processToServer (FiatMoveSubmittedBy (FiatPlayer 0)) initSettingsMsg (Just initialStateMsg) invalidMove :: NoGameFiatGameState) `shouldBe` ToServer.Error ToClient.InvalidMove
    it "game is not started"
      $ runIdentity (processToServer (FiatMoveSubmittedBy (FiatPlayer 0)) initSettingsMsg Nothing invalidMove :: NoGameFiatGameState) `shouldBe` ToServer.Error ToClient.GameIsNotStarted
    it "game already started"
      $ runIdentity (processToServer (FiatMoveSubmittedBy System) initSettingsMsg (Just initialStateMsg) startGame :: NoGameFiatGameState) `shouldBe` ToServer.Error ToClient.GameAlreadyStarted
    it "decode error"
      $ runIdentity (processToServer (FiatMoveSubmittedBy (FiatPlayer 1)) initSettingsMsg (Just $ FiatGameStateMsg "") (FiatToServertMsg "") :: NoGameFiatGameState) `shouldBe` ToServer.Error (ToClient.DecodeError "Error in $: not enough input")
  describe "addPlayer" $ do
    it "good"
      $ goodSettings `shouldBe` Just (NoSettings [FiatPlayer 1, FiatPlayer 0] False ())
    it "game is full"
      $ badSettings `shouldBe` Nothing
  describe "toClientMsg" $ do
    it "good - ToServer.MsgProcessed"
      $ runIdentity goodToClientMsg `shouldBe` ToClient.Msg (SettingsAndState initClientSettings (Just $ GameState Playing (ClientNoGame False) Nothing))
    it "good - SettingsAndState s Nothing"
      $ runIdentity (toClientMsg (FiatPlayer 1) (Right (SettingsAndState initSettings Nothing))) `shouldBe` ToClient.Msg (SettingsAndState initClientSettings Nothing)
    it "good - SettingsAndState s (Just gs)"
      $ runIdentity (toClientMsg (FiatPlayer 1) (Right (SettingsAndState  initSettings (Just initialState)))) `shouldBe` ToClient.Msg (SettingsAndState initClientSettings (Just initialClientState))
