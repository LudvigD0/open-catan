{-# LANGUAGE NamedFieldPuns #-}
module WebSocket
  -- ( runLocalClient
  -- , runLocalClientWith
  -- , runLocalServer 
  -- , runLocalServerWith
  -- ) 
  where

import qualified Types as OCTypes
import           Control.Monad (
  forM_
  -- , forever
  )

import           Control.Concurrent (
  MVar
  --, newMVar, modifyMVar_, modifyMVar, readMVar
  )

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

-- function arg reminders:
{-
runClient :: String       -- ^ Host
          -> Int          -- ^ Port
          -> String       -- ^ Path
          -> ClientApp a  -- ^ Client application
          -> IO a

runClientWith :: String             -- ^ Host
              -> Int                -- ^ Port
              -> String             -- ^ Path
              -> ConnectionOptions  -- ^ Options
              -> Headers            -- ^ Custom headers to send
              -> ClientApp a        -- ^ Client application
              -> IO a
-}


-- "Data types and utils" ===================================================================

data Client = Client
  { playerId :: OCTypes.PlayerId
  , conn     :: WS.Connection
  -- , isHost :: Bool -- is this required?
  } 

-- Get the Clients ID
getClientID :: Client -> OCTypes.PlayerId
getClientID (Client { playerId }) = playerId

getClientConnection :: Client -> WS.Connection
getClientConnection (Client { conn }) = conn

-- ---------- !!! NOTE about the Client Container !!!! -----------
-- Can eventually be replaced with a map structure.
data ServerState = ServerState 
  { curUsers  :: [Client]
  }
  
-- Create a new, initial state:
newServerState :: ServerState 
newServerState = ServerState 
  { curUsers = []
  }

-- Get the number of active clients:
numClients :: ServerState -> Int
numClients (ServerState {curUsers}) = length curUsers

-- Check if a user already exists (based on PlayerId):
clientExists :: Client -> ServerState -> Bool
clientExists client (ServerState {curUsers}) = 
  any ((== getClientID client) . getClientID)

-- Get Clients
getCurrentClients :: ServerState -> [Client]
getCurrentClients (ServerState {curUsers}) = curUsers

-- Add a client (this does not check if the client already exists, you should do
-- this yourself using `clientExists`):
addClient :: Client -> ServerState -> ServerState
addClient client state = state {
    curUsers = (client : getCurrentClients state)
  }
  -- client : clients
  

-- Remove a client:
removeClient :: Client -> ServerState -> ServerState
removeClient client state = state {
    curUsers = filter ((/= getClientID client) . getClientID) $ getCurrentClients state
  }

-- Send a message to all clients, and log it on stdout:
broadcast :: Text -> ServerState -> IO ()
broadcast message state = do
    T.putStrLn message
    let clients = getCurrentClients state
    in forM_ clients $ \(Client { conn }) -> WS.sendTextData conn message


-- Constants ===================================================================
-- From "server":   ws://localhost:9160
c_localHost :: String
c_localHost = "0.0.0.0"
c_localPort :: Int
c_localPort = 9160
c_localPath :: String
c_localPath = "/"

data ServerAddress = ServerAddress 
  { host :: String
  , port :: Int
  , path :: String
  }

localAddress = ServerAddress 
  { host = c_localHost
  , port = c_localPort
  , path = c_localPath
  }

-- Client ======================================================================
runLocalClient :: WS.ClientApp a -> IO a
runLocalClient = WS.runClient c_localHost c_localPort c_localPath

runLocalClientWith :: WS.ConnectionOptions
                   -> WS.Headers
                   -> WS.ClientApp a
                   -> IO a
runLocalClientWith = WS.runClientWith c_localHost c_localPort c_localPath

-- Server ======================================================================

-- Check out Wai for glue for providing a better server than
-- runServer/-WithOptions


{-
  type ServerApp = PendingConnection -> IO ()

  runServer
    :: String     Address to bind
    -> Int        Port to listen on
    -> ServerApp  Application
    -> IO ()      Never returns
-}


runLocalServer :: WS.ServerApp -> IO ()
runLocalServer = WS.runServer c_localHost c_localPort

runLocalServerWith :: WS.ConnectionOptions -> WS.ServerApp  -> IO a
runLocalServerWith connectOpts = 
  WS.runServerWithOptions WS.defaultServerOptions {
    WS.serverHost = c_localHost,
    WS.serverPort = c_localPort,
    WS.serverConnectionOptions = connectOpts
  }


{-
data OCServerApp = OCServerApp {
  clients :: MVar ServerState
}


appWrapper :: OCServerApp -> WS.ServerApp
appWrapper app pend = undefined


--runServerEX :: OCServerApp -> IO
runServerEX (ServerAddress {host, port}) app =
  WS.runServer 
    host
    port
    $ appWrapper app


-- a stage that accepts players
  -- allow chat?
-- a stage that only is for game logic.
-}
