{-# LANGUAGE OverloadedStrings #-}
module Main 
    ( main
    ) where

import qualified WebSocket as OCWS -- Our module (Open Catan Web Socket)
import qualified Types as OCTypes
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Control.Concurrent (threadDelay)
import qualified Data.Text as T
-- Direct access to the internal websockets functions.
-- Should be replaced with our WebSockets eventually.
-- Currently used for applying settings, sending/receiving messages 
-- and keeping the connection alive.
import           Network.WebSockets as WS

-- MAIN CONTENT ================================================================
-- =============================================================================

main :: IO ()
main = do
  putStrLn "Startar på ws://localhost:9160"
  OCWS.runLocalServerWith 
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


textAsJson :: T.Text -> LC8.ByteString
textAsJson = LC8.pack . T.unpack

printWSMessage :: Maybe OCTypes.WSMessage -> IO()
printWSMessage Nothing = putStrLn "wrong type"
printWSMessage (Just msg) = case msg of
  OCTypes.PkgBoardStatus     _ -> putStrLn "Not handled yet."
  OCTypes.PkgGameAction action -> putStrLn $ show action

loop :: Connection -> IO ()
loop conn = do
  msg <- receiveData conn :: IO T.Text
  
  putStrLn ("Received: " ++ T.unpack msg)
  printWSMessage (decode (textAsJson msg) :: Maybe OCTypes.WSMessage)

  sendTextData conn ("Echo: " <> msg)
  --timer

  sendTextData conn (msg)
  let new = T.reverse msg
  sendTextData conn (new)

  loop conn
