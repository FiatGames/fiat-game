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
import           Data.Proxy
import           Data.Text                 (Text)
import           Data.Text.Encoding
import           FiatGame.Class
import           FiatGame.GameState
import qualified FiatGame.ToClient.Types   as ToClient
import qualified FiatGame.ToServer.Types   as ToServer
import qualified NoGame                    as NoGame
import           Test.Hspec


type NoGameToServerMsg = ToServer.Msg NoGame.Settings NoGame.Move

initClientSettings :: NoGame.ClientSettings
initClientSettings = NoGame.ClientSettings [] False
initSettingsMsg :: SettingsMsg
initSettingsMsg = SettingsMsg $ decodeUtf8 $ toStrict $ encode NoGame.initSettings
changedSettings :: NoGame.Settings
changedSettings = NoGame.Settings [] True ()
twoFiatPlayersSettings :: NoGame.Settings
twoFiatPlayersSettings = NoGame.Settings [FiatPlayer 0, FiatPlayer 1] False ()
twoFiatPlayerSettingsMsg :: SettingsMsg
twoFiatPlayerSettingsMsg = SettingsMsg $ decodeUtf8 $ toStrict $ encode twoFiatPlayersSettings
twoFiatPlayersSettingsAfter :: NoGame.Settings
twoFiatPlayersSettingsAfter = NoGame.Settings [FiatPlayer 0, FiatPlayer 1] True ()

goodSettings :: Maybe NoGame.Settings
goodSettings = runIdentity $ do
  i <- defaultSettings
  runMaybeT $ foldM (\s p -> MaybeT $ addPlayer p s) i [FiatPlayer 0, FiatPlayer 1]
badSettings :: Maybe NoGame.Settings
badSettings = runIdentity $ do
    i <- defaultSettings
    runMaybeT $ foldM (\s p -> MaybeT $ addPlayer p s) i [FiatPlayer 0, FiatPlayer 1, FiatPlayer 2]

initialState  :: GameState NoGame.GameState NoGame.Move
initialState = GameState Playing (NoGame.GameState True ()) Nothing
initialClientState  :: GameState NoGame.ClientGameState NoGame.Move
initialClientState = GameState Playing (NoGame.ClientGameState True) Nothing
initialStateMsg :: GameStateMsg
initialStateMsg = GameStateMsg $ decodeUtf8 $ toStrict $ encode initialState
systemMove :: ToServerMsg
systemMove = ToServerMsg $ decodeUtf8 $ toStrict $ encode (ToServer.Msg System (ToServer.MakeMove NoGame.ToB) :: NoGameToServerMsg)
goodMove :: ToServerMsg
goodMove = ToServerMsg $ decodeUtf8 $ toStrict $ encode (ToServer.Msg (FiatPlayer 0) (ToServer.MakeMove NoGame.ToB) :: NoGameToServerMsg)
invalidMove :: ToServerMsg
invalidMove = ToServerMsg $ decodeUtf8 $ toStrict $ encode (ToServer.Msg (FiatPlayer 0) (ToServer.MakeMove NoGame.ToA) :: NoGameToServerMsg)
unauthorizedMove :: ToServerMsg
unauthorizedMove = ToServerMsg $ decodeUtf8 $ toStrict $ encode (ToServer.Msg (FiatPlayer 1) (ToServer.MakeMove NoGame.ToB) :: NoGameToServerMsg)
startGame :: ToServerMsg
startGame =  ToServerMsg $ decodeUtf8 $ toStrict $ encode (ToServer.Msg System ToServer.StartGame :: NoGameToServerMsg)
updateSettings :: ToServerMsg
updateSettings =  ToServerMsg $ decodeUtf8 $ toStrict $ encode (ToServer.Msg System (ToServer.UpdateSettings changedSettings) :: NoGameToServerMsg)

--Helper for all tests
process :: FiatGame m NoGame.GameState NoGame.Settings NoGame.Move NoGame.ClientGameState NoGame.ClientSettings => FiatPlayer -> SettingsMsg -> Maybe GameStateMsg -> ToServerMsg -> m (ChannelMsg, Maybe (GameStage,FromFiat))
process p s mgs = processToServer (Proxy :: Proxy NoGame.Settings) (MoveSubmittedBy p) (s, mgs)

--SUCESS
successResult :: NoGame.Settings -> Maybe (GameState NoGame.GameState NoGame.Move) -> (ChannelMsg, Maybe (GameStage,FromFiat))
successResult s mgs = (ChannelMsg (toStrict (encode (Right (SettingsAndState s mgs) :: NoGame.Processed))), Just (stage, (SettingsMsg $ decodeUtf8 $ toStrict $ encode s, GameStateMsg . decodeUtf8 . toStrict . encode <$> mgs)))
  where stage = maybe SettingUp (\(FiatGame.GameState.GameState st _ _) -> st) mgs

goodProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat))
goodProcessToServer = process (FiatPlayer 0) initSettingsMsg (Just initialStateMsg) goodMove
goodToClientMsg :: Identity ToClientMsg
goodToClientMsg = goodProcessToServer >>= toClientMsg (Proxy :: Proxy NoGame.Settings) (FiatPlayer 0)  . fst
startGameProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat))
startGameProcessToServer = process System twoFiatPlayerSettingsMsg Nothing startGame
updateSettingsProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat))
updateSettingsProcessToServer = process System initSettingsMsg Nothing updateSettings
systemAllowedProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat))
systemAllowedProcessToServer = process System initSettingsMsg (Just initialStateMsg) systemMove
moveOnOthersBehalfProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat))
moveOnOthersBehalfProcessToServer = process System initSettingsMsg (Just initialStateMsg) goodMove


foo = ChannelMsg $ toStrict $ encode (Right (SettingsAndState NoGame.initSettings Nothing) :: NoGame.Processed)
foo2 = ChannelMsg $ toStrict $ encode (Right (SettingsAndState NoGame.initSettings (Just initialState)) ::  NoGame.Processed)


--FAILURES
failResult :: ToClient.Error -> (ChannelMsg, Maybe (GameStage,FromFiat))
failResult err = (ChannelMsg (toStrict (encode (Left err ::  NoGame.Processed))), Nothing)

failedToStartProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat))
failedToStartProcessToServer = process System initSettingsMsg Nothing startGame
unauthorizedProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat))
unauthorizedProcessToServer = process (FiatPlayer 0) initSettingsMsg (Just initialStateMsg) unauthorizedMove
notYourTurnProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat))
notYourTurnProcessToServer = process (FiatPlayer 1) initSettingsMsg (Just initialStateMsg) unauthorizedMove
invalidProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat))
invalidProcessToServer = process (FiatPlayer 0) initSettingsMsg (Just initialStateMsg) invalidMove
gameNotStartedProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat))
gameNotStartedProcessToServer = process (FiatPlayer 0) initSettingsMsg Nothing invalidMove
gameAlreadyStartedProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat))
gameAlreadyStartedProcessToServer = process System initSettingsMsg (Just initialStateMsg) startGame
decodeErrorProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat))
decodeErrorProcessToServer = process (FiatPlayer 1) initSettingsMsg (Just (GameStateMsg "")) (ToServerMsg "")

main :: IO ()
main = hspec $ do
  describe "processToServer" $ do
    it "good"
      $ runIdentity goodProcessToServer `shouldBe` successResult NoGame.initSettings (Just $ GameState Playing (NoGame.GameState False ()) Nothing)
    it "start game"
      $ runIdentity startGameProcessToServer `shouldBe` successResult twoFiatPlayersSettingsAfter (Just $ GameState Playing (NoGame.GameState True ()) Nothing)
    it "update settings"
      $ runIdentity updateSettingsProcessToServer `shouldBe` successResult changedSettings Nothing
    it "system allowed"
      $ runIdentity systemAllowedProcessToServer `shouldBe` successResult NoGame.initSettings (Just $ GameState Playing (NoGame.GameState False ()) Nothing)
    it "system allowed to move on other's behalf"
      $ runIdentity moveOnOthersBehalfProcessToServer `shouldBe` successResult NoGame.initSettings (Just $ GameState Playing (NoGame.GameState False ()) Nothing)
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
  describe "addFiatPlayer" $ do
    it "good"
      $ goodSettings `shouldBe` Just (NoGame.Settings [FiatPlayer 1, FiatPlayer 0] False ())
    it "game is full"
      $ badSettings `shouldBe` Nothing
  describe "toClientMsg" $ do
    it "good - ToServer.MsgProcessed"
      $ runIdentity goodToClientMsg `shouldBe` ToClientMsg (decodeUtf8 (toStrict $ encode (ToClient.Msg $ SettingsAndState initClientSettings $ Just $ GameState Playing (NoGame.ClientGameState False) Nothing :: NoGame.ClientMsg)))
    it "good - SettingsAndState s Nothing"
      $ runIdentity ( toClientMsg (Proxy :: Proxy NoGame.Settings) (FiatPlayer 1) foo) `shouldBe` ToClientMsg (decodeUtf8 (toStrict $ encode (ToClient.Msg (SettingsAndState initClientSettings Nothing) :: NoGame.ClientMsg)))
    it "good - SettingsAndState s (Just gs)"
      $ runIdentity ( toClientMsg (Proxy :: Proxy NoGame.Settings) (FiatPlayer 1) foo2) `shouldBe` ToClientMsg (decodeUtf8 (toStrict $ encode (ToClient.Msg (SettingsAndState initClientSettings (Just initialClientState)) :: NoGame.ClientMsg)))
