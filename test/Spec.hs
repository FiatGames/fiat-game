{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
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

type NoGameFiatGameState = Either ToClient.Error (SettingsAndState NoSettings NoGame NoMoves)
type NoGameClientMsg = ToClient.Msg ClientNoSettings ClientNoGame NoMoves
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
systemMove :: FiatToServerMsg
systemMove = FiatToServerMsg $ decodeUtf8 $ toStrict $ encode (ToServer.Msg System (ToServer.MakeMove ToB) :: NoGameToServerMsg)
goodMove :: FiatToServerMsg
goodMove = FiatToServerMsg $ decodeUtf8 $ toStrict $ encode (ToServer.Msg (FiatPlayer 0) (ToServer.MakeMove ToB) :: NoGameToServerMsg)
invalidMove :: FiatToServerMsg
invalidMove = FiatToServerMsg $ decodeUtf8 $ toStrict $ encode (ToServer.Msg (FiatPlayer 0) (ToServer.MakeMove ToA) :: NoGameToServerMsg)
unauthorizedMove :: FiatToServerMsg
unauthorizedMove = FiatToServerMsg $ decodeUtf8 $ toStrict $ encode (ToServer.Msg (FiatPlayer 1) (ToServer.MakeMove ToB) :: NoGameToServerMsg)
startGame :: FiatToServerMsg
startGame =  FiatToServerMsg $ decodeUtf8 $ toStrict $ encode (ToServer.Msg System ToServer.StartGame :: NoGameToServerMsg)
updateSettings :: FiatToServerMsg
updateSettings =  FiatToServerMsg $ decodeUtf8 $ toStrict $ encode (ToServer.Msg System (ToServer.UpdateSettings changedSettings) :: NoGameToServerMsg)

--Helper for all tests
process :: FiatGame m NoGame NoSettings NoMoves ClientNoGame ClientNoSettings => FiatPlayer -> FiatGameSettingsMsg -> Maybe FiatGameStateMsg -> FiatToServerMsg -> m (FiatGameChannelMsg, Maybe FromFiat)
process p s mgs = processToServer (FiatMoveSubmittedBy p) (undefined :: NoSettings) (s, mgs)

--SUCESS
successResult :: NoSettings -> Maybe (GameState NoGame NoMoves) -> (FiatGameChannelMsg,Maybe (FiatGameSettingsMsg, Maybe FiatGameStateMsg))
successResult s mgs = (FiatGameChannelMsg (toStrict (encode (Right (SettingsAndState s mgs) :: NoGameFiatGameState))), Just (FiatGameSettingsMsg $ decodeUtf8 $ toStrict $ encode s, FiatGameStateMsg . decodeUtf8 . toStrict . encode <$> mgs))

goodProcessToServer :: Identity (FiatGameChannelMsg, Maybe FromFiat)
goodProcessToServer = process (FiatPlayer 0) initSettingsMsg (Just initialStateMsg) goodMove
goodToClientMsg :: Identity Text
goodToClientMsg = goodProcessToServer >>= toClientMsg (FiatPlayer 0) (undefined :: NoSettings) . fst
startGameProcessToServer :: Identity (FiatGameChannelMsg, Maybe FromFiat)
startGameProcessToServer = process System twoPlayerSettingsMsg Nothing startGame
updateSettingsProcessToServer :: Identity (FiatGameChannelMsg, Maybe FromFiat)
updateSettingsProcessToServer = process System initSettingsMsg Nothing updateSettings
systemAllowedProcessToServer :: Identity (FiatGameChannelMsg, Maybe FromFiat)
systemAllowedProcessToServer = process System initSettingsMsg (Just initialStateMsg) systemMove
moveOnOthersBehalfProcessToServer :: Identity (FiatGameChannelMsg, Maybe FromFiat)
moveOnOthersBehalfProcessToServer = process System initSettingsMsg (Just initialStateMsg) goodMove

--FAILURES
failResult :: ToClient.Error -> (FiatGameChannelMsg, Maybe a)
failResult err = (FiatGameChannelMsg (toStrict (encode (Left err :: NoGameFiatGameState))), Nothing)

failedToStartProcessToServer :: Identity (FiatGameChannelMsg, Maybe FromFiat)
failedToStartProcessToServer = process System initSettingsMsg Nothing startGame
unauthorizedProcessToServer :: Identity (FiatGameChannelMsg, Maybe FromFiat)
unauthorizedProcessToServer = process (FiatPlayer 0) initSettingsMsg (Just initialStateMsg) unauthorizedMove
notYourTurnProcessToServer :: Identity (FiatGameChannelMsg, Maybe FromFiat)
notYourTurnProcessToServer = process (FiatPlayer 1) initSettingsMsg (Just initialStateMsg) unauthorizedMove
invalidProcessToServer :: Identity (FiatGameChannelMsg, Maybe FromFiat)
invalidProcessToServer = process (FiatPlayer 0) initSettingsMsg (Just initialStateMsg) invalidMove
gameNotStartedProcessToServer :: Identity (FiatGameChannelMsg, Maybe FromFiat)
gameNotStartedProcessToServer = process (FiatPlayer 0) initSettingsMsg Nothing invalidMove
gameAlreadyStartedProcessToServer :: Identity (FiatGameChannelMsg, Maybe FromFiat)
gameAlreadyStartedProcessToServer = process System initSettingsMsg (Just initialStateMsg) startGame
decodeErrorProcessToServer :: Identity (FiatGameChannelMsg, Maybe FromFiat)
decodeErrorProcessToServer = process (FiatPlayer 1) initSettingsMsg (Just (FiatGameStateMsg "")) (FiatToServerMsg "")

main :: IO ()
main = hspec $ do
  describe "processToServer" $ do
    it "good"
      $ runIdentity goodProcessToServer `shouldBe` successResult initSettings (Just $ GameState Playing (NoGame False ()) Nothing)
    it "start game"
      $ runIdentity startGameProcessToServer `shouldBe` successResult twoPlayersSettingsAfter (Just $ GameState Playing (NoGame True ()) Nothing)
    it "update settings"
      $ runIdentity updateSettingsProcessToServer `shouldBe` successResult changedSettings Nothing
    it "system allowed"
      $ runIdentity systemAllowedProcessToServer `shouldBe` successResult initSettings (Just $ GameState Playing (NoGame False ()) Nothing)
    it "system allowed to move on other's behalf"
      $ runIdentity moveOnOthersBehalfProcessToServer `shouldBe` successResult initSettings (Just $ GameState Playing (NoGame False ()) Nothing)
    it "failed to start game"
      $ runIdentity failedToStartProcessToServer `shouldBe` failResult (ToClient.FailedToInitialize "Not enough players")
    it "unauthorized"
      $ runIdentity unauthorizedProcessToServer `shouldBe` failResult ToClient.Unauthorized
    it "not your turn"
      $ runIdentity notYourTurnProcessToServer `shouldBe` failResult ToClient.NotYourTurn
    it "invalid"
      $ runIdentity invalidProcessToServer `shouldBe` failResult ToClient.InvalidMove
    it "game is not started"
      $ runIdentity gameNotStartedProcessToServer `shouldBe` failResult ToClient.GameIsNotStarted
    it "game already started"
      $ runIdentity gameAlreadyStartedProcessToServer `shouldBe` failResult ToClient.GameAlreadyStarted
    it "decode error"
      $ runIdentity decodeErrorProcessToServer `shouldBe` failResult (ToClient.DecodeError "Error in $: not enough input")
  describe "addPlayer" $ do
    it "good"
      $ goodSettings `shouldBe` Just (NoSettings [FiatPlayer 1, FiatPlayer 0] False ())
    it "game is full"
      $ badSettings `shouldBe` Nothing
  describe "toClientMsg" $ do
    it "good - ToServer.MsgProcessed"
      $ runIdentity goodToClientMsg `shouldBe` decodeUtf8 (toStrict $ encode (ToClient.Msg $ SettingsAndState initClientSettings $ Just $ GameState Playing (ClientNoGame False) Nothing :: NoGameClientMsg))
    it "good - SettingsAndState s Nothing"
      $ runIdentity ( toGameChannelMsg (Right(SettingsAndState initSettings Nothing)) >>= toClientMsg (FiatPlayer 1) (undefined :: NoSettings)) `shouldBe` decodeUtf8 (toStrict $ encode (ToClient.Msg (SettingsAndState initClientSettings Nothing) :: NoGameClientMsg))
    it "good - SettingsAndState s (Just gs)"
      $ runIdentity (toGameChannelMsg (Right (SettingsAndState initSettings (Just initialState))) >>= toClientMsg (FiatPlayer 1) (undefined :: NoSettings)) `shouldBe` decodeUtf8 (toStrict $ encode (ToClient.Msg (SettingsAndState initClientSettings (Just initialClientState)) :: NoGameClientMsg))
