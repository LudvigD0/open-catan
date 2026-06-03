{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main 
    ( main
    ) where

import qualified WebSocket as OCWS -- Our module (Open Catan Web Socket)
import qualified Types as OCTypes
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Control.Concurrent (threadDelay)
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- Direct access to the internal websockets functions.
-- Should be replaced with our WebSockets eventually.
-- Currently used for applying settings, sending/receiving messages 
-- and keeping the connection alive.
import           Network.WebSockets as WS


import Control.Concurrent 
  ( MVar
  , newMVar
  , modifyMVar_
  , modifyMVar
  , readMVar
  )
import Control.Monad (
  -- forM_, 
  forever)


-- "LAB" CONTENT ===============================================================
-- =============================================================================
-- Experimental code meant for understanding functions and/or exploring solutions

{-
timer :: IO ()
timer = do
  putStrLn "Tick!"
  threadDelay (10 * 1000000) -- 10 sekunder
  timer
-}

{-
textAsJson :: T.Text -> LC8.ByteString
textAsJson = LC8.pack . T.unpack
-}


-- ###### For rejection
{-
There is technically no "close connection"
There is a rejectRequest function, but that is more likely used for https responses

We can just drop the connections though. That's how the example handles it.

potential reject codes:
  423 Locked

-}



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

main_draft :: IO ()
main_draft = do 
  putStrLn "Startar på ws://localhost:9160"

  -- state <- newMVar newServerState
  state <- newMVar newLabServerState

  OCWS.runLocalServerWith 
    WS.defaultConnectionOptions {
      WS.connectionStrictUnicode = True,
      WS.connectionFramePayloadSizeLimit = WS.NoSizeLimit -- WS.SizeLimit 32
    }
    $ app_draft state

  -- WS.runServer "127.0.0.1" 9160 $ application state



app :: PendingConnection -> IO ()
app pending = do
  -- "prints the requestHeaders of the pending connection"
  putStrLn $ show $ pendingRequest pending 
  conn <- acceptRequest pending
  putStrLn "Client connected!"
  
  sendTextData conn ("Connected to Haskell server!" :: T.Text)

  loop conn


type HandleWSMessage = OCTypes.WSMessage -> IO ()

tryInterpretWSMessage :: HandleWSMessage -> Maybe OCTypes.WSMessage -> IO ()
tryInterpretWSMessage _      Nothing    = putStrLn "wrong type"
tryInterpretWSMessage handle (Just msg) = handle msg

printWSMessage :: HandleWSMessage
printWSMessage msg = case msg of
  OCTypes.PkgBoardStatus     _ -> putStrLn "Not handled yet."
  OCTypes.PkgGameAction action -> putStrLn $ show action


interpretDataMsg :: Connection -> IO()
interpretDataMsg conn = do
    msg <- WS.receiveDataMessage conn
    case msg of 
      (WS.Text   bstring _) -> putStrLn $ LC8.unpack bstring
      (WS.Binary bstring)   -> tryInterpretWSMessage printWSMessage (decode bstring)


loop :: Connection -> IO ()
loop conn = do
  interpretDataMsg conn
  
  -- msg <- receiveData conn :: IO T.Text
  -- putStrLn ("Received: " ++ T.unpack msg)
  -- printWSMessage (decode (textAsJson msg) :: Maybe OCTypes.WSMessage)

  -- sendTextData conn ("Echo: " <> msg)
  -- --timer

  -- sendTextData conn (msg)
  -- let new = T.reverse msg
  -- sendTextData conn (new)

  loop conn


acceptNewUser_noApp :: WS.PendingConnection -> IO (WS.Connection)
acceptNewUser_noApp pending = do 
  -- Update Serverstate with a new user
  
  -- wait for someone to join.
  conn <- acceptRequest pending
  
  -- (optional) - welcome the new client
  putStrLn "Client connected!"
  WS.sendTextData conn ("Connected to Haskell server!" :: T.Text)

  return conn

{-
-}
acceptNewUser :: OCWS.ServerState -> IO (OCWS.ServerState)
acceptNewUser state = do
  -- conn <- acceptRequest pending

  return lsData'
    where 
      lsData' = lsData {
        numUsers = nextNum lsData
        }
      nextNum = \(LabServerData{ numUsers }) -> numUsers



-- keep "LabServerData" as an mutex
app_draft :: MVar LabServerData -> WS.PendingConnection -> IO()
app_draft state pending = do
  conn <- WS.acceptRequest pending
  -- let timeoutDur = 30
  -- let noIOop = return ()
  WS.withPingThread conn 30 (return ()) $ do
    msg <- WS.receiveData conn
    T.putStrLn msg
    (LabServerData {curUsers}) <- readMVar state
    let userCount = length curUsers

    putStrLn $ "userCount: " ++ show userCount
    -- let num = length curUsers
    case userCount of 
      _ | 2 <= userCount ->
            -- reject user, as we have reaced our limit
            WS.sendTextData conn ("Lobby is full" :: T.Text)
        | otherwise -> do
            -- accept new user
            modifyMVar_ state $ \s -> do
              let s' = s {
                curUsers = client:curUsers
              }
              return s'
            WS.sendTextData conn ("You were accepted" :: T.Text)
            client_loop client state
       where
        client     = (userCount + 1, conn)
    
      
  -- accept a user
  -- { hide direct call ?}
  -- "conn <- acceptRequest pending"
  
  -- (?) is it necessary to use WS.withPingThread?

  -- if we do not accept users
    -- send an explanation to them. ("lobby is full")
  -- else
    -- "poke" mutex serverState
    -- create their UUID
    -- send welcome message to them and their UUID
    -- add them to serverState
    -- prep "disconnect" callback
    -- put them in a loop

    -- server keeps track of which client can change state
      -- { make the client a collection of data and methods, then let server change methods of clients? }
  -- putStrLn "closing out"


-- type ClientData = (Int, WS.Connection)





-- talk :: Client -> MVar ServerState -> IO ()
client_loop :: ClientData -> MVar LabServerData -> IO ()
client_loop (_, conn) _ = forever $ do

  -- wait to receive data from client
  -- when msg has been received:
  -- read state to get current "handle function"
  -- 

  -- temp stand-in
  msg <- WS.receiveData conn
  T.putStrLn msg
  -- readMVar state >>= broadcast (user `mappend` ": " `mappend` msg)


{-
app_draft :: PendingConnection -> IO ()
app_draft pending = do
  -- wait for someone to join.
  conn <- acceptNewUser_noApp pending

  -- put the client in a loop
  loop_draft (0, conn)

loop_draft :: ClientData -> IO ()
loop_draft (state, conn) = do
  -- wait for a message
  -- msg <- WS.receiveDataMessage conn
  interpretDataMsg conn

  loop_draft (state, conn)

-}





