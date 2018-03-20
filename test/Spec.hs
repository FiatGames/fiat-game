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

day0 :: UTCTime
day0 = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

futureMoveGoodMsg :: FutureMoveMsg
futureMoveGoodMsg = FutureMoveMsg $ decodeUtf8 $ toStrict $ encode $ FutureMove day0 NoGame.ToB
futureMoveBadMsg :: FutureMoveMsg
futureMoveBadMsg = FutureMoveMsg $ decodeUtf8 $ toStrict $ encode $ FutureMove day0 NoGame.ToA


--SUCESS
successResult :: NoGame.Settings -> Maybe (GameState NoGame.GameState NoGame.Move) -> (ChannelMsg, Maybe (GameStage,FromFiat,Maybe (UTCTime, FutureMoveMsg)))
successResult s mgs
  = (ChannelMsg (toStrict (encode (Right (SettingsAndState s mgs) :: NoGame.Processed))), Just (stage, (SettingsMsg $ decodeUtf8 $ toStrict $ encode s, GameStateMsg . decodeUtf8 . toStrict . encode <$> mgs), over _2 (FutureMoveMsg . decodeUtf8 . toStrict . encode) <$> fMv))
  where
    fMv = fmap (\f -> (timeForFutureMove f, f)) (join (futureMove <$> mgs))
    stage = maybe SettingUp (\(FiatGame.GameState.GameState st _ _) -> st) mgs

goodProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat,Maybe (UTCTime, FutureMoveMsg)))
goodProcessToServer = NoGame.processToServer (MoveSubmittedBy (FiatPlayer 0)) (initSettingsMsg,Just initialStateMsg) goodMove
goodToClientMsg :: Identity ToClientMsg
goodToClientMsg = goodProcessToServer >>= NoGame.toClientMsg (FiatPlayer 0)  . fst
startGameProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat,Maybe (UTCTime, FutureMoveMsg)))
startGameProcessToServer = NoGame.processToServer (MoveSubmittedBy System) (twoFiatPlayerSettingsMsg,Nothing) startGame
updateSettingsProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat,Maybe (UTCTime, FutureMoveMsg)))
updateSettingsProcessToServer = NoGame.processToServer (MoveSubmittedBy System) (initSettingsMsg, Nothing) updateSettings
systemAllowedProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat,Maybe (UTCTime, FutureMoveMsg)))
systemAllowedProcessToServer = NoGame.processToServer (MoveSubmittedBy System) (initSettingsMsg, Just initialStateMsg) systemMove
moveOnOthersBehalfProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat,Maybe (UTCTime, FutureMoveMsg)))
moveOnOthersBehalfProcessToServer = NoGame.processToServer (MoveSubmittedBy System) (initSettingsMsg, Just initialStateMsg) goodMove

toClientMsgGoodNothing :: Identity ToClientMsg
toClientMsgGoodNothing = NoGame.toClientMsg  (FiatPlayer 1) $ ChannelMsg $ toStrict $ encode (Right (SettingsAndState NoGame.initSettings Nothing) :: NoGame.Processed)
toClientMsgGoodJust :: Identity ToClientMsg
toClientMsgGoodJust = NoGame.toClientMsg (FiatPlayer 1) $ ChannelMsg $ toStrict $ encode (Right (SettingsAndState NoGame.initSettings (Just initialState)) ::  NoGame.Processed)

futureMoveGood :: Identity (ChannelMsg, Maybe (GameStage,FromFiat,Maybe (UTCTime, FutureMoveMsg)))
futureMoveGood = NoGame.proccessFutureMove (initSettingsMsg,Just initialStateMsg) futureMoveGoodMsg

--FAILURES
failResult :: FiatPlayer -> ToClient.Error -> (ChannelMsg, Maybe (GameStage,FromFiat,Maybe (UTCTime, FutureMoveMsg)))
failResult p err = (ChannelMsg (toStrict (encode (Left (p,err) :: NoGame.Processed))), Nothing)

failedToStartProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat,Maybe (UTCTime, FutureMoveMsg)))
failedToStartProcessToServer = NoGame.processToServer (MoveSubmittedBy System) (initSettingsMsg, Nothing) startGame
unauthorizedProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat,Maybe (UTCTime, FutureMoveMsg)))
unauthorizedProcessToServer = NoGame.processToServer (MoveSubmittedBy (FiatPlayer 0)) (initSettingsMsg, Just initialStateMsg) unauthorizedMove
notYourTurnProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat,Maybe (UTCTime, FutureMoveMsg)))
notYourTurnProcessToServer = NoGame.processToServer (MoveSubmittedBy (FiatPlayer 1)) (initSettingsMsg, Just initialStateMsg) unauthorizedMove
invalidProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat,Maybe (UTCTime, FutureMoveMsg)))
invalidProcessToServer = NoGame.processToServer (MoveSubmittedBy (FiatPlayer 0)) (initSettingsMsg, Just initialStateMsg) invalidMove
gameNotStartedProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat,Maybe (UTCTime, FutureMoveMsg)))
gameNotStartedProcessToServer = NoGame.processToServer (MoveSubmittedBy (FiatPlayer 0)) (initSettingsMsg, Nothing) invalidMove
gameAlreadyStartedProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat,Maybe (UTCTime, FutureMoveMsg)))
gameAlreadyStartedProcessToServer = NoGame.processToServer (MoveSubmittedBy System) (initSettingsMsg, Just initialStateMsg) startGame
decodeErrorProcessToServer :: Identity (ChannelMsg, Maybe (GameStage,FromFiat,Maybe (UTCTime, FutureMoveMsg)))
decodeErrorProcessToServer = NoGame.processToServer (MoveSubmittedBy (FiatPlayer 1)) (initSettingsMsg, Just (GameStateMsg "")) (ToServerMsg "")

futureMoveInvalid :: Identity (ChannelMsg, Maybe (GameStage,FromFiat,Maybe (UTCTime, FutureMoveMsg)))
futureMoveInvalid = NoGame.proccessFutureMove (initSettingsMsg,Just initialStateMsg) futureMoveBadMsg
futureMoveDecodeError :: Identity (ChannelMsg, Maybe (GameStage,FromFiat,Maybe (UTCTime, FutureMoveMsg)))
futureMoveDecodeError = NoGame.proccessFutureMove (initSettingsMsg, Just (GameStateMsg "")) (FutureMoveMsg "")

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
      $ runIdentity failedToStartProcessToServer `shouldBe` failResult System (ToClient.FailedToInitialize "Not enough players")
    it "unauthorized"
      $ runIdentity unauthorizedProcessToServer `shouldBe` failResult (FiatPlayer 0) ToClient.Unauthorized
    it "not your turn"
      $ runIdentity notYourTurnProcessToServer `shouldBe` failResult (FiatPlayer 1) ToClient.NotYourTurn
    it "invalid"
      $ runIdentity invalidProcessToServer `shouldBe` failResult (FiatPlayer 0) ToClient.InvalidMove
    it "game is not started"
      $ runIdentity gameNotStartedProcessToServer `shouldBe` failResult (FiatPlayer 0) ToClient.GameIsNotStarted
    it "game already started"
      $ runIdentity gameAlreadyStartedProcessToServer `shouldBe` failResult System ToClient.GameAlreadyStarted
    it "decode error"
      $ runIdentity decodeErrorProcessToServer `shouldBe` failResult (FiatPlayer 1) (ToClient.DecodeError "Error in $: not enough input")
  describe "addFiatPlayer" $ do
    it "good"
      $ goodSettings `shouldBe` Just (NoGame.Settings [FiatPlayer 1, FiatPlayer 0] False ())
    it "game is full"
      $ badSettings `shouldBe` Nothing
  describe "toClientMsg" $ do
    it "good - ToServer.MsgProcessed"
      $ runIdentity goodToClientMsg `shouldBe` ToClientMsg (decodeUtf8 (toStrict $ encode (ToClient.Msg $ SettingsAndState initClientSettings $ Just $ GameState Playing (NoGame.ClientGameState False) Nothing :: NoGame.ClientMsg)))
    it "good - SettingsAndState s Nothing"
      $ runIdentity toClientMsgGoodNothing `shouldBe` ToClientMsg (decodeUtf8 (toStrict $ encode (ToClient.Msg (SettingsAndState initClientSettings Nothing) :: NoGame.ClientMsg)))
    it "good - SettingsAndState s (Just gs)"
      $ runIdentity toClientMsgGoodJust `shouldBe` ToClientMsg (decodeUtf8 (toStrict $ encode (ToClient.Msg (SettingsAndState initClientSettings (Just initialClientState)) :: NoGame.ClientMsg)))
  describe "processFutureMove" $ do
      it "good"
       $ runIdentity futureMoveGood `shouldBe` successResult NoGame.initSettings (Just $ GameState Playing (NoGame.GameState False ()) Nothing)
      it "invalid"
       $ runIdentity futureMoveInvalid `shouldBe` failResult System ToClient.InvalidMove
      it "decode error"
       $ runIdentity futureMoveDecodeError `shouldBe` failResult System (ToClient.DecodeError "Error in $: not enough input")
