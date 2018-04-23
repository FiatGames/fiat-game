{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.ByteString.Lazy      (toStrict)
import           Data.Maybe
import qualified Data.Text                 as T
import           Data.Text.Encoding
import           Data.Time.Clock
import           FiatGame.Class
import qualified FiatGame.ToClient.Types   as ToClient
import qualified FiatGame.ToServer.Types   as ToServer
import           FiatGame.Types
import qualified NoGame
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
initialState = GameState Playing (NoGame.GameState True () 0) Nothing
initialClientState  :: GameState NoGame.ClientGameState NoGame.Move
initialClientState = GameState Playing (NoGame.ClientGameState True) Nothing
initialStateMsg :: GameStateMsg
initialStateMsg = GameStateMsg $ encodeToText initialState
systemMove :: ToServerMsg
systemMove = ToServerMsg $ encodeToText (ToServer.Msg System (ToServer.MakeMove NoGame.ToB) (FiatGameHash "") :: NoGameToServerMsg)
goodMove :: ToServerMsg
goodMove = ToServerMsg $ encodeToText (ToServer.Msg (FiatPlayer 0) (ToServer.MakeMove NoGame.ToB) (FiatGameHash "0") :: NoGameToServerMsg)
invalidMove :: ToServerMsg
invalidMove = ToServerMsg $ encodeToText (ToServer.Msg (FiatPlayer 0) (ToServer.MakeMove NoGame.ToA) (FiatGameHash "0") :: NoGameToServerMsg)
unauthorizedMove :: ToServerMsg
unauthorizedMove = ToServerMsg $ encodeToText (ToServer.Msg (FiatPlayer 1) (ToServer.MakeMove NoGame.ToB) (FiatGameHash "0") :: NoGameToServerMsg)
startGame :: ToServerMsg
startGame =  ToServerMsg $ encodeToText (ToServer.Msg System ToServer.StartGame (FiatGameHash "") :: NoGameToServerMsg)
updateSettings :: ToServerMsg
updateSettings =  ToServerMsg $ encodeToText (ToServer.Msg System (ToServer.UpdateSettings changedSettings) (FiatGameHash "") :: NoGameToServerMsg)

--SUCESS
successResult :: NoGame.Settings -> Maybe (GameState NoGame.GameState NoGame.Move) -> Processed
successResult s mgs
  = Processed (ToFiatMsg (encodeToText (ToClient.Msg hash s mgs :: ToClient.Msg NoGame.Settings NoGame.GameState NoGame.Move))) (Just successfulProcess) False
  where
    hash = FiatGameHash $ fromMaybe "" $ getHash <$> mgs
    getHash (GameState _ (NoGame.GameState _ _ t) _) =  T.pack $ show t
    successfulProcess = SuccessfulProcessed stage fromfiat (over _2 (ToServerMsg . encodeToText) <$> fMv)
    fromfiat = FromFiat (SettingsMsg $ encodeToText s) (GameStateMsg . encodeToText <$> mgs) hash
    fMv :: Maybe (UTCTime, ToServer.Msg NoGame.Settings NoGame.Move)
    fMv = fmap (\f -> (f^.futureMoveTime, ToServer.Msg System (ToServer.MakeMove (f^.futureMoveMove)) hash)) (join (view gameStateFutureMove <$> mgs))
    stage = maybe SettingUp (\(FiatGame.Types.GameState st _ _) -> st) mgs

goodProcessToServer :: IO Processed
goodProcessToServer = NoGame.processToServer (MoveSubmittedBy (FiatPlayer 0)) (FromFiat initSettingsMsg (Just initialStateMsg) (FiatGameHash "0")) goodMove
goodToClientMsg :: IO ToClientMsg
goodToClientMsg = goodProcessToServer >>= NoGame.toClientMsg (FiatPlayer 0)  . view processedToClientMsg
startGameProcessToServer :: IO Processed
startGameProcessToServer = NoGame.processToServer (MoveSubmittedBy System) (FromFiat twoFiatPlayerSettingsMsg Nothing (FiatGameHash "")) startGame
updateSettingsProcessToServer :: IO Processed
updateSettingsProcessToServer = NoGame.processToServer (MoveSubmittedBy System) (FromFiat initSettingsMsg Nothing (FiatGameHash "")) updateSettings
systemAllowedProcessToServer :: IO Processed
systemAllowedProcessToServer = NoGame.processToServer (MoveSubmittedBy System) (FromFiat initSettingsMsg (Just initialStateMsg) (FiatGameHash "")) systemMove
moveOnOthersBehalfProcessToServer :: IO Processed
moveOnOthersBehalfProcessToServer = NoGame.processToServer (MoveSubmittedBy System) (FromFiat initSettingsMsg (Just initialStateMsg) (FiatGameHash "0")) goodMove

toClientMsgGoodNothing :: IO ToClientMsg
toClientMsgGoodNothing = NoGame.toClientMsg  (FiatPlayer 1) $ ToFiatMsg $ encodeToText (ToClient.Msg (FiatGameHash "") NoGame.initSettings Nothing :: ToClient.Msg NoGame.Settings NoGame.GameState NoGame.Move )
toClientMsgGoodJust :: IO ToClientMsg
toClientMsgGoodJust = NoGame.toClientMsg (FiatPlayer 1) $ ToFiatMsg $ encodeToText (ToClient.Msg (FiatGameHash "") NoGame.initSettings (Just initialState) ::  ToClient.Msg NoGame.Settings NoGame.GameState NoGame.Move)

--FAILURES
failResult :: FiatPlayer -> ToClient.Error -> Processed
failResult p err = Processed (ToFiatMsg $ encodeToText (ToClient.Error p err :: NoGame.ClientMsg)) Nothing (case err of ToClient.GameStateOutOfDate -> True; _ -> False)

failedToStartProcessToServer :: IO Processed
failedToStartProcessToServer = NoGame.processToServer (MoveSubmittedBy System) (FromFiat initSettingsMsg Nothing (FiatGameHash "")) startGame
unauthorizedProcessToServer :: IO Processed
unauthorizedProcessToServer = NoGame.processToServer (MoveSubmittedBy (FiatPlayer 0)) (FromFiat initSettingsMsg (Just initialStateMsg) (FiatGameHash "")) unauthorizedMove
notYourTurnProcessToServer :: IO Processed
notYourTurnProcessToServer = NoGame.processToServer (MoveSubmittedBy (FiatPlayer 1)) (FromFiat initSettingsMsg (Just initialStateMsg) (FiatGameHash "0")) unauthorizedMove
invalidProcessToServer :: IO Processed
invalidProcessToServer = NoGame.processToServer (MoveSubmittedBy (FiatPlayer 0)) (FromFiat initSettingsMsg (Just initialStateMsg) (FiatGameHash "0")) invalidMove
gameNotStartedProcessToServer :: IO Processed
gameNotStartedProcessToServer = NoGame.processToServer (MoveSubmittedBy (FiatPlayer 0)) (FromFiat initSettingsMsg Nothing (FiatGameHash "0")) invalidMove
gameAlreadyStartedProcessToServer :: IO Processed
gameAlreadyStartedProcessToServer = NoGame.processToServer (MoveSubmittedBy System) (FromFiat initSettingsMsg (Just initialStateMsg) (FiatGameHash "")) startGame
decodeErrorProcessToServer :: IO Processed
decodeErrorProcessToServer = NoGame.processToServer (MoveSubmittedBy (FiatPlayer 1)) (FromFiat initSettingsMsg (Just (GameStateMsg "")) (FiatGameHash "")) (ToServerMsg "")

main :: IO ()
main = hspec $ do
  describe "processToServer" $ do
    it "good"
      $  goodProcessToServer `shouldReturn` successResult NoGame.initSettings (Just $ GameState Playing (NoGame.GameState False () 1) Nothing)
    it "start game"
      $  startGameProcessToServer `shouldReturn` successResult twoFiatPlayersSettingsAfter (Just $ GameState Playing (NoGame.GameState True () 0) Nothing)
    it "update settings"
      $  updateSettingsProcessToServer `shouldReturn` successResult changedSettings Nothing
    it "system allowed"
      $  systemAllowedProcessToServer `shouldReturn` successResult NoGame.initSettings (Just $ GameState Playing (NoGame.GameState False () 1) Nothing)
    it "system allowed to move on other's behalf"
      $  moveOnOthersBehalfProcessToServer `shouldReturn` successResult NoGame.initSettings (Just $ GameState Playing (NoGame.GameState False () 1) Nothing)
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
      $  goodToClientMsg `shouldReturn` ToClientMsg (encodeToText (ToClient.Msg (FiatGameHash "1") initClientSettings (Just $ GameState Playing (NoGame.ClientGameState False) Nothing) :: NoGame.ClientMsg))
    it "good - SettingsAndState s Nothing"
      $  toClientMsgGoodNothing `shouldReturn` ToClientMsg (encodeToText (ToClient.Msg (FiatGameHash "") initClientSettings Nothing :: NoGame.ClientMsg))
    it "good - SettingsAndState s (Just gs)"
      $  toClientMsgGoodJust `shouldReturn` ToClientMsg (encodeToText (ToClient.Msg (FiatGameHash "") initClientSettings (Just initialClientState) :: NoGame.ClientMsg))
