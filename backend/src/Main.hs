{-Tanken med denna fil är att den ska starta hela appen
ladda config, öppna databasanslutning, initiera states, starta HTTP + Websocket-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.WebSockets
import qualified Data.Text as T
import Control.Concurrent (threadDelay)


main :: IO ()
main = do
  putStrLn "Startar på ws://localhost:9160"
  runServer "0.0.0.0" 9160 app

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

  --sendTextData conn ("Echo: " <> msg)
  timer

  sendTextData conn (msg)
  let new = T.reverse msg
  sendTextData conn (new)

  loop conn
