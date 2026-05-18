{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

-- Libs
import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant

-- Local
import GameActions
import Types

-- Respose which represent successful or failed request
data GameResponse
  = GameStateResponse GameState
  | GameErrorResponse GameError
  deriving (Generic)

instance ToJSON GameResponse

-- Global game state, curretly only a single instance of the game 
type GameStore = MVar (Maybe GameState)

{-  Curretly 3 requests 
POST /game/start    -- Start new game
GET  /game          -- Get current state
POST /game/action { action: GameAction } -- Try GameAction 
 -}
type API =
       "game" :> "start"
              :> Post '[JSON] GameResponse
  :<|> "game" :> Get '[JSON] GameResponse
  :<|> "game" :> "action"
              :> ReqBody '[JSON] GameAction
              :> Post '[JSON] GameResponse

--------------------------------------- Handlers -------------------------------------------------
-- Starts new game instance 
startGameHandler :: GameStore -> Handler GameResponse
startGameHandler store = liftIO $ do
    let gs = startGame
    modifyMVar_ store $ \_ -> return (Just gs)
    return $ GameStateResponse gs

-- Get current gamestate
getGameHandler :: GameStore -> Handler GameResponse
getGameHandler store = liftIO $ do
    currentGame <- readMVar store
    return $ case currentGame of
        Nothing -> GameErrorResponse GameNotStarted
        Just gs -> GameStateResponse gs

-- Call coresponding gameaction handler for request GameAction, returns new gamestate
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

server :: GameStore -> Server API
server store =
    startGameHandler store
    :<|> getGameHandler store
    :<|> actionHandler store

api :: Proxy API
api = Proxy

app :: GameStore -> Application
app store = serve api (server store)

main :: IO ()
main = do
    store <- newMVar Nothing
    putStrLn "Running on http://localhost:8080"
    putStrLn "  POST /game/start"
    putStrLn "  GET  /game"
    putStrLn "  POST /game/action"
    run 8080 (app store)
    