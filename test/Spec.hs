{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Control.Lens
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.ByteString.Lazy      (toStrict)
import           Data.Text                 (Text)
import           Data.Text.Encoding
import           Data.Time.Calendar
import           Data.Time.Clock
import           FiatGame.Class
import qualified FiatGame.ToClient.Types   as ToClient
import qualified FiatGame.ToServer.Types   as ToServer
import           FiatGame.Types
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

goodSettings :: IO (Maybe NoGame.Settings)
goodSettings = do
  i <- defaultSettings
  runMaybeT $ foldM (\s p -> MaybeT $ addPlayer p s) i [FiatPlayer 0, FiatPlayer 1]
badSettings :: IO (Maybe NoGame.Settings)
badSettings = do
    i <- defaultSettings
    runMaybeT $ foldM (\s p -> MaybeT $ addPlayer p s) i [FiatPlayer 0, FiatPlayer 1, FiatPlayer 2]

initialState  :: GameState NoGame.GameState NoGame.Move
initialState = GameState Playing (NoGame.GameState True ()) Nothing
initialClientState  :: GameState NoGame.ClientGameState NoGame.Move
initialClientState = GameState Playing (NoGame.ClientGameState True) Nothing
initialStateMsg :: GameStateMsg
initialStateMsg = GameStateMsg $ encodeToText initialState
systemMove :: ToServerMsg
systemMove = ToServerMsg $ encodeToText (ToServer.Msg System (ToServer.MakeMove NoGame.ToB) (FiatGameHash "abc") :: NoGameToServerMsg)
goodMove :: ToServerMsg
goodMove = ToServerMsg $ encodeToText (ToServer.Msg (FiatPlayer 0) (ToServer.MakeMove NoGame.ToB) (FiatGameHash "abc") :: NoGameToServerMsg)
invalidMove :: ToServerMsg
invalidMove = ToServerMsg $ encodeToText (ToServer.Msg (FiatPlayer 0) (ToServer.MakeMove NoGame.ToA) (FiatGameHash "abc") :: NoGameToServerMsg)
unauthorizedMove :: ToServerMsg
unauthorizedMove = ToServerMsg $ encodeToText (ToServer.Msg (FiatPlayer 1) (ToServer.MakeMove NoGame.ToB) (FiatGameHash "abc") :: NoGameToServerMsg)
startGame :: ToServerMsg
startGame =  ToServerMsg $ encodeToText (ToServer.Msg System ToServer.StartGame (FiatGameHash "abc") :: NoGameToServerMsg)
updateSettings :: ToServerMsg
updateSettings =  ToServerMsg $ encodeToText (ToServer.Msg System (ToServer.UpdateSettings changedSettings) (FiatGameHash "abc") :: NoGameToServerMsg)

day0 :: UTCTime
day0 = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)


--SUCESS
successResult :: NoGame.Settings -> Maybe (GameState NoGame.GameState NoGame.Move) -> Processed
successResult s mgs
  = Processed (ToFiatMsg (encodeToText (ToClient.Msg (FiatGameHash "abc") s mgs :: ToClient.Msg NoGame.Settings NoGame.GameState NoGame.Move))) (Just successfulProcess)
  where
    successfulProcess = SuccessfulProcessed stage fromfiat (over _2 (ToServerMsg . encodeToText) <$> fMv)
    fromfiat = FromFiat (SettingsMsg $ encodeToText s) (GameStateMsg . encodeToText <$> mgs) (FiatGameHash "abc")
    fMv :: Maybe (UTCTime, ToServer.Msg NoGame.Settings NoGame.Move)
    fMv = fmap (\f -> (f^.futureMoveTime, ToServer.Msg System (ToServer.MakeMove (f^.futureMoveMove)) (FiatGameHash "abc"))) (join (futureMove <$> mgs))
    stage = maybe SettingUp (\(FiatGame.Types.GameState st _ _) -> st) mgs

goodProcessToServer :: IO Processed
goodProcessToServer = NoGame.processToServer (MoveSubmittedBy (FiatPlayer 0)) (FromFiat initSettingsMsg (Just initialStateMsg) (FiatGameHash "abc")) goodMove
goodToClientMsg :: IO ToClientMsg
goodToClientMsg = goodProcessToServer >>= NoGame.toClientMsg (FiatPlayer 0)  . view processedToClientMsg
startGameProcessToServer :: IO Processed
startGameProcessToServer = NoGame.processToServer (MoveSubmittedBy System) (FromFiat twoFiatPlayerSettingsMsg Nothing (FiatGameHash "abc")) startGame
updateSettingsProcessToServer :: IO Processed
updateSettingsProcessToServer = NoGame.processToServer (MoveSubmittedBy System) (FromFiat initSettingsMsg Nothing (FiatGameHash "abc")) updateSettings
systemAllowedProcessToServer :: IO Processed
systemAllowedProcessToServer = NoGame.processToServer (MoveSubmittedBy System) (FromFiat initSettingsMsg (Just initialStateMsg) (FiatGameHash "abc")) systemMove
moveOnOthersBehalfProcessToServer :: IO Processed
moveOnOthersBehalfProcessToServer = NoGame.processToServer (MoveSubmittedBy System) (FromFiat initSettingsMsg (Just initialStateMsg) (FiatGameHash "abc")) goodMove

toClientMsgGoodNothing :: IO ToClientMsg
toClientMsgGoodNothing = NoGame.toClientMsg  (FiatPlayer 1) $ ToFiatMsg $ encodeToText (ToClient.Msg (FiatGameHash "abc") NoGame.initSettings Nothing :: ToClient.Msg NoGame.Settings NoGame.GameState NoGame.Move )
toClientMsgGoodJust :: IO ToClientMsg
toClientMsgGoodJust = NoGame.toClientMsg (FiatPlayer 1) $ ToFiatMsg $ encodeToText (ToClient.Msg (FiatGameHash "abc") NoGame.initSettings (Just initialState) ::  ToClient.Msg NoGame.Settings NoGame.GameState NoGame.Move)

--FAILURES
failResult :: FiatPlayer -> ToClient.Error -> Processed
failResult p err = Processed (ToFiatMsg $ encodeToText (ToClient.Error p err :: NoGame.ClientMsg)) Nothing

failedToStartProcessToServer :: IO Processed
failedToStartProcessToServer = NoGame.processToServer (MoveSubmittedBy System) (FromFiat initSettingsMsg Nothing (FiatGameHash "abc")) startGame
unauthorizedProcessToServer :: IO Processed
unauthorizedProcessToServer = NoGame.processToServer (MoveSubmittedBy (FiatPlayer 0)) (FromFiat initSettingsMsg (Just initialStateMsg) (FiatGameHash "abc")) unauthorizedMove
notYourTurnProcessToServer :: IO Processed
notYourTurnProcessToServer = NoGame.processToServer (MoveSubmittedBy (FiatPlayer 1)) (FromFiat initSettingsMsg (Just initialStateMsg) (FiatGameHash "abc")) unauthorizedMove
invalidProcessToServer :: IO Processed
invalidProcessToServer = NoGame.processToServer (MoveSubmittedBy (FiatPlayer 0)) (FromFiat initSettingsMsg (Just initialStateMsg) (FiatGameHash "abc")) invalidMove
gameNotStartedProcessToServer :: IO Processed
gameNotStartedProcessToServer = NoGame.processToServer (MoveSubmittedBy (FiatPlayer 0)) (FromFiat initSettingsMsg Nothing (FiatGameHash "abc")) invalidMove
gameAlreadyStartedProcessToServer :: IO Processed
gameAlreadyStartedProcessToServer = NoGame.processToServer (MoveSubmittedBy System) (FromFiat initSettingsMsg (Just initialStateMsg) (FiatGameHash "abc")) startGame
decodeErrorProcessToServer :: IO Processed
decodeErrorProcessToServer = NoGame.processToServer (MoveSubmittedBy (FiatPlayer 1)) (FromFiat initSettingsMsg (Just (GameStateMsg "")) (FiatGameHash "abc")) (ToServerMsg "")

main :: IO ()
main = hspec $ do
  describe "processToServer" $ do
    it "good"
      $  goodProcessToServer `shouldReturn` successResult NoGame.initSettings (Just $ GameState Playing (NoGame.GameState False ()) Nothing)
    it "start game"
      $  startGameProcessToServer `shouldReturn` successResult twoFiatPlayersSettingsAfter (Just $ GameState Playing (NoGame.GameState True ()) Nothing)
    it "update settings"
      $  updateSettingsProcessToServer `shouldReturn` successResult changedSettings Nothing
    it "system allowed"
      $  systemAllowedProcessToServer `shouldReturn` successResult NoGame.initSettings (Just $ GameState Playing (NoGame.GameState False ()) Nothing)
    it "system allowed to move on other's behalf"
      $  moveOnOthersBehalfProcessToServer `shouldReturn` successResult NoGame.initSettings (Just $ GameState Playing (NoGame.GameState False ()) Nothing)
    it "failed to start game"
      $  failedToStartProcessToServer `shouldReturn` failResult System (ToClient.FailedToInitialize "Not enough players")
    it "unauthorized"
      $  unauthorizedProcessToServer `shouldReturn` failResult (FiatPlayer 0) ToClient.Unauthorized
    it "not your turn"
      $  notYourTurnProcessToServer `shouldReturn` failResult (FiatPlayer 1) ToClient.NotYourTurn
    it "invalid"
      $  invalidProcessToServer `shouldReturn` failResult (FiatPlayer 0) ToClient.InvalidMove
    it "game is not started"
      $  gameNotStartedProcessToServer `shouldReturn` failResult (FiatPlayer 0) ToClient.GameIsNotStarted
    it "game already started"
      $  gameAlreadyStartedProcessToServer `shouldReturn` failResult System ToClient.GameAlreadyStarted
    it "decode error"
      $  decodeErrorProcessToServer `shouldReturn` failResult (FiatPlayer 1) (ToClient.DecodeError "Error in $: not enough input")
  describe "addFiatPlayer" $ do
    it "good"
      $ goodSettings `shouldReturn` Just (NoGame.Settings [FiatPlayer 1, FiatPlayer 0] False ())
    it "game is full"
      $ badSettings `shouldReturn` Nothing
  describe "toClientMsg" $ do
    it "good - ToServer.MsgProcessed"
      $  goodToClientMsg `shouldReturn` ToClientMsg (encodeToText (ToClient.Msg (FiatGameHash "abc") initClientSettings (Just $ GameState Playing (NoGame.ClientGameState False) Nothing) :: NoGame.ClientMsg))
    it "good - SettingsAndState s Nothing"
      $  toClientMsgGoodNothing `shouldReturn` ToClientMsg (encodeToText (ToClient.Msg (FiatGameHash "abc") initClientSettings Nothing :: NoGame.ClientMsg))
    it "good - SettingsAndState s (Just gs)"
      $  toClientMsgGoodJust `shouldReturn` ToClientMsg (encodeToText (ToClient.Msg (FiatGameHash "abc") initClientSettings (Just initialClientState) :: NoGame.ClientMsg))
