module WebSocket
  ( runLocalClient
  , runLocalClientWith
  , runLocalServer 
  , runLocalServerWith
  ) where

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

-- Constants ===================================================================
-- From "server":   ws://localhost:9160
c_localHost :: String
c_localHost = "0.0.0.0"
c_localPort :: Int
c_localPort = 9160
c_localPath :: String
c_localPath = "/"

-- Client ======================================================================
runLocalClient :: WS.ClientApp a -> IO a
runLocalClient = WS.runClient c_localHost c_localPort c_localPath

runLocalClientWith :: WS.ConnectionOptions
                   -> WS.Headers
                   -> WS.ClientApp a
                   -> IO a
runLocalClientWith = WS.runClientWith c_localHost c_localPort c_localPath

-- Server ======================================================================
runLocalServer :: WS.ServerApp -> IO ()
runLocalServer = WS.runServer c_localHost c_localPort

runLocalServerWith :: WS.ConnectionOptions -> WS.ServerApp  -> IO a
runLocalServerWith connectOpts = 
  WS.runServerWithOptions WS.defaultServerOptions {
    WS.serverHost = c_localHost,
    WS.serverPort = c_localPort,
    WS.serverConnectionOptions = connectOpts
  } 