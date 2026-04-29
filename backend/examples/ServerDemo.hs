{-# LANGUAGE OverloadedStrings #-}
module Main 
    ( main
    ) where

import            Control.Concurrent (threadDelay)
import qualified  Data.Text as T
import            Network.WebSockets as WS
import            Foobar -- test import and module
--import            WebSocket  -- our module

-- !!!
-- Until I've worked out how cabal wants you to define local modules and imports

-- Constants ===================================================================
-- From "server":   ws://localhost:9160
c_localHost :: String
c_localHost = "0.0.0.0"
c_localPort :: Int
c_localPort = 9160
-- c_localPath :: String -- unused here
-- c_localPath = "/"

-- Server ======================================================================
runLocalServer :: ServerApp -> IO ()
runLocalServer = runServer c_localHost c_localPort

runLocalServerWith :: ConnectionOptions -> ServerApp  -> IO a
runLocalServerWith connectOpts = 
  runServerWithOptions defaultServerOptions {
    serverHost = c_localHost,
    serverPort = c_localPort,
    serverConnectionOptions = connectOpts
  } 


-- MAIN CONTENT ================================================================
-- =============================================================================

main :: IO ()
main = do
  putStrLn "Startar på ws://localhost:9160"
  runLocalServerWith 
    WS.defaultConnectionOptions {
      WS.connectionStrictUnicode = True,
      WS.connectionFramePayloadSizeLimit = WS.NoSizeLimit -- WS.SizeLimit 32
    }
    app

app :: PendingConnection -> IO ()
app pending = do
  conn <- acceptRequest pending
  putStrLn "Client connected!"
  
  sendTextData conn ("Connected to Haskell server!" :: T.Text)

  loop conn


timer :: IO ()
timer = do
  putStrLn "Tick!"
  threadDelay (10 * 1000000) -- 10 sekunder
  timer

loop :: Connection -> IO ()
loop conn = do
  msg <- receiveData conn :: IO T.Text
  putStrLn ("Received: " ++ T.unpack msg)

  sendTextData conn ("Echo: " <> msg)
  --timer

  sendTextData conn (msg)
  let new = T.reverse msg
  sendTextData conn (new)

  loop conn
