-- | Local WebSocket client and server helpers.
module WebSocket
  ( runLocalClient
  , runLocalClientWith
  , runLocalServer 
  , runLocalServerWith
  ) where

import qualified Network.WebSockets as WS

-- | Host used by local websocket demos.
c_localHost :: String
c_localHost = "0.0.0.0"

-- | Port used by local websocket demos.
c_localPort :: Int
c_localPort = 9160

-- | Path used by local websocket demos.
c_localPath :: String
c_localPath = "/"

-- | Run a websocket client against the local demo server.
runLocalClient :: WS.ClientApp a -> IO a
runLocalClient = WS.runClient c_localHost c_localPort c_localPath

-- | Run a websocket client with custom connection options and headers.
runLocalClientWith :: WS.ConnectionOptions
                   -> WS.Headers
                   -> WS.ClientApp a
                   -> IO a
runLocalClientWith = WS.runClientWith c_localHost c_localPort c_localPath

-- | Run a websocket server on the local demo host and port.
runLocalServer :: WS.ServerApp -> IO ()
runLocalServer = WS.runServer c_localHost c_localPort

-- | Run a websocket server with custom connection options.
runLocalServerWith :: WS.ConnectionOptions -> WS.ServerApp  -> IO a
runLocalServerWith connectOpts = 
  WS.runServerWithOptions WS.defaultServerOptions {
    WS.serverHost = c_localHost,
    WS.serverPort = c_localPort,
    WS.serverConnectionOptions = connectOpts
  } 
