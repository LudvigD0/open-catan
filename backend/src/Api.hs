{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | HTTP API executable for the Catan backend.
module Main where

import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Servant

import GameActions
import Types

-- | Mutable storage for the currently running game.
--
-- The backend currently hosts a single game instance.
type GameStore = MVar (Maybe GameState)

-- | Public HTTP routes exposed by the backend.
--
-- * POST /game/start starts a new game.
-- * GET /game returns the current game state.
-- * POST /game/action applies a 'GameAction' to the current game.
type API =
       "game" :> "start"
              :> Post '[JSON] GameResponse
  :<|> "game" :> Get '[JSON] GameResponse
  :<|> "game" :> "action"
              :> ReqBody '[JSON] GameAction
              :> Post '[JSON] GameResponse

-- | Start a new game and replace any existing state.
startGameHandler :: GameStore -> Handler GameResponse
startGameHandler store = liftIO $ do
    let gs = startGame
    modifyMVar_ store $ \_ -> return (Just gs)
    return $ GameStateResponse gs

-- | Return the current game state, or 'GameNotStarted' when none exists.
getGameHandler :: GameStore -> Handler GameResponse
getGameHandler store = liftIO $ do
    currentGame <- readMVar store
    return $ case currentGame of
        Nothing -> GameErrorResponse GameNotStarted
        Just gs -> GameStateResponse gs

-- | Apply a requested game action atomically to the stored game state.
actionHandler :: GameStore -> GameAction -> Handler GameResponse
actionHandler store action = liftIO $
    modifyMVar store $ \currentGame ->
        case currentGame of
            Nothing ->
                return (Nothing, GameErrorResponse GameNotStarted)
            Just gs -> do
                result <- applyGameAction action gs
                case result of
                    Left err ->
                        return (Just gs, GameErrorResponse err)
                    Right gs' ->
                        return (Just gs', GameStateResponse gs')

-- | Servant server implementation for 'API'.
server :: GameStore -> Server API
server store =
    startGameHandler store
    :<|> getGameHandler store
    :<|> actionHandler store

-- | Type-level API witness used by Servant.
api :: Proxy API
api = Proxy

-- | CORS policy for the local frontend development server.
corsPolicy :: CorsResourcePolicy
corsPolicy = simpleCorsResourcePolicy
    { corsOrigins = Just (["http://127.0.0.1:8081", "http://localhost:8081"], True)
    , corsMethods = ["GET", "POST", "OPTIONS"]
    , corsRequestHeaders = ["Content-Type"]
    }

-- | WAI application for the Catan backend.
app :: GameStore -> Application
app store = cors (const $ Just corsPolicy) $ serve api (server store)

-- | Run the HTTP API server on port 8080.
main :: IO ()
main = do
    store <- newMVar Nothing
    putStrLn "Running on http://localhost:8080"
    putStrLn "  POST /game/start"
    putStrLn "  GET  /game"
    putStrLn "  POST /game/action"
    run 8080 (app store)
    
