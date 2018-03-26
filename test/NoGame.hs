{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module NoGame where
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Proxy
import qualified Data.Text               as T
import           FiatGame.Class          as FiatGame
import qualified FiatGame.ToClient.Types as ToClient
import qualified FiatGame.Types          as FiatGame

data GameState = GameState
  { turn      :: Bool
  , gsSecret  :: ()
  , gsTurnNum :: Int
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
$(deriveJSON defaultOptions ''NoGame.ClientSettings)

data Move = ToA | ToB
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''NoGame.Move)

type ClientMsg = ToClient.Msg NoGame.ClientSettings ClientGameState NoGame.Move
type State = FiatGame.GameState GameState NoGame.Move

initSettings :: Settings
initSettings = Settings [] False ()

instance FiatGame Settings where
  type Move Settings = NoGame.Move
  type State Settings = NoGame.GameState
  type ClientState Settings = NoGame.ClientGameState
  type ClientSettings Settings = NoGame.ClientSettings

  defaultSettings = return initSettings
  addPlayer p (Settings ps c ())
    | length ps < 2 = return $ Just (Settings (p:ps) c ())
    | otherwise = return Nothing
  makeMove _ _ (FiatGame.GameState stage (GameState True _ t) _) _ = return $ FiatGame.GameState stage (GameState False () (t+1)) Nothing
  makeMove _ _ (FiatGame.GameState stage (GameState False _ t) _) _ = return $ FiatGame.GameState stage (GameState True () (t+1)) Nothing
  isMoveValid _ _ (FiatGame.GameState _ (GameState True _ _) _) ToB  = return True
  isMoveValid _ _ (FiatGame.GameState _ (GameState False _ _) _) ToA = return True
  isMoveValid _ _ _ _                                  = return False
  isPlayersTurn (FiatGame.FiatPlayer 0) _ (FiatGame.GameState _ (GameState True () _) _) _ = return True
  isPlayersTurn (FiatGame.FiatPlayer 1) _ (FiatGame.GameState _ (GameState False _ _) _) _ = return True
  isPlayersTurn _ _ _ _                                         = return False
  initialGameState (Settings ps c ())
    | length ps < 2 = return $ Left "Not enough players"
    | otherwise = return $ Right (Settings ps (not c) (), FiatGame.GameState FiatGame.Playing (GameState True () 0) Nothing)
  toClientSettingsAndState _ (Settings ps c _) (Just (FiatGame.GameState stage (GameState b () _) mvs))= return (ClientSettings ps c, Just (FiatGame.GameState stage (ClientGameState b) mvs))
  toClientSettingsAndState _ (Settings ps c _) Nothing = return (ClientSettings ps c, Nothing)
  newHash _ Nothing = return $ FiatGame.FiatGameHash ""
  newHash _ (Just (FiatGame.GameState _ (GameState _ _ t) _)) = return $ FiatGame.FiatGameHash $ T.pack $ show t

processToServer :: FiatGame.MoveSubmittedBy -> FiatGame.FromFiat -> FiatGame.ToServerMsg -> IO FiatGame.Processed
processToServer = FiatGame.processToServer (Proxy :: Proxy Settings)
toClientMsg :: FiatGame.FiatPlayer -> FiatGame.ToFiatMsg -> IO FiatGame.ToClientMsg
toClientMsg = FiatGame.toClientMsg (Proxy :: Proxy Settings)
