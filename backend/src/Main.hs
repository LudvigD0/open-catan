{-Tanken med denna fil är att den ska starta hela appen
ladda config, öppna databasanslutning, initiera states, starta HTTP + Websocket-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.WebSockets
import qualified Data.Text as T

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

loop :: Connection -> IO ()
loop conn = do
  msg <- receiveData conn :: IO T.Text
  putStrLn ("Received: " ++ T.unpack msg)

  sendTextData conn ("Echo: " <> msg)

  loop conn