{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module NoGame where
import           Control.Monad.Identity
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Proxy
import           Data.Text               (Text)
import           Data.Time.Clock
import           FiatGame.Class          as FiatGame
import qualified FiatGame.GameState      as FiatGame
import qualified FiatGame.ToClient.Types as ToClient

data GameState = GameState
  { turn     :: Bool
  , gsSecret :: ()
  }
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''GameState)

data ClientGameState = ClientGameState
  { cTurn     :: Bool
  }
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''ClientGameState)

data Settings = Settings
  { players              :: [FiatGame.FiatPlayer]
  , changeMe             :: Bool
  , itsASecretToEveryone :: ()
  }
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''Settings)

data ClientSettings = ClientSettings
  { csFiatPlayers :: [FiatGame.FiatPlayer]
  , csChangeMe    :: Bool
  }
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''ClientSettings)

data Move = ToA | ToB
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''Move)

type ClientMsg = ToClient.Msg ClientSettings ClientGameState Move
type State = FiatGame.GameState GameState Move

initSettings :: Settings
initSettings = Settings [] False ()

instance FiatGame Identity GameState Settings Move ClientGameState ClientSettings where
  defaultSettings = return initSettings
  addPlayer p (Settings ps c ())
    | length ps < 2 = return $ Just (Settings (p:ps) c ())
    | otherwise = return Nothing
  makeMove _ _ (FiatGame.GameState stage (GameState True _) _) _ = return $ FiatGame.GameState stage (GameState False ()) Nothing
  makeMove _ _ (FiatGame.GameState stage (GameState False _) _) _ = return $ FiatGame.GameState stage (GameState True ()) Nothing
  isMoveValid _ _ (FiatGame.GameState _ (GameState True _) _) ToB  = return True
  isMoveValid _ _ (FiatGame.GameState _ (GameState False _) _) ToA = return True
  isMoveValid _ _ _ _                                  = return False
  isPlayersTurn (FiatGame.FiatPlayer 0) _ (FiatGame.GameState _ (GameState True ()) _) _ = return True
  isPlayersTurn (FiatGame.FiatPlayer 1) _ (FiatGame.GameState _ (GameState False _) _) _ = return True
  isPlayersTurn _ _ _ _                                         = return False
  initialGameState (Settings ps c ())
    | length ps < 2 = return $ Left "Not enough players"
    | otherwise = return $ Right (Settings ps (not c) (), FiatGame.GameState FiatGame.Playing (GameState True ()) Nothing)
  toClientSettingsAndState _ (FiatGame.SettingsAndState (Settings ps c _) (Just (FiatGame.GameState stage (GameState b ()) mvs))) = return (FiatGame.SettingsAndState (ClientSettings ps c) (Just (FiatGame.GameState stage (ClientGameState b) mvs)))
  toClientSettingsAndState _ (FiatGame.SettingsAndState (Settings ps c _) Nothing) = return (FiatGame.SettingsAndState (ClientSettings ps c) Nothing)
  newHash _ = return $ FiatGame.FiatGameHash "abc"

proccessFutureMove :: FromFiat -> FutureMoveMsg -> Identity Processed
proccessFutureMove = FiatGame.proccessFutureMove (Proxy :: Proxy Settings)
processToServer :: MoveSubmittedBy -> FromFiat -> ToServerMsg -> Identity Processed
processToServer = FiatGame.processToServer (Proxy :: Proxy Settings)
toClientMsg :: FiatGame.FiatPlayer -> ToChannelMsg -> Identity ToClientMsg
toClientMsg = FiatGame.toClientMsg (Proxy :: Proxy Settings)
