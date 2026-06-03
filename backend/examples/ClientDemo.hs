{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DeriveAnyClass #-}

module Main 
    ( main
    ) where

import qualified WebSocket as OCWS -- Our module (Open Catan Web Socket)
import qualified Types as OCTypes
import           Data.Aeson
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
-- Direct access to the internal websockets functions.
-- Should be replaced with our WebSockets eventually.
-- Currently used for applying settings, sending/receiving messages 
-- and keeping the connection alive.
import qualified Network.WebSockets  as WS


-- MAIN CONTENT ================================================================
-- =============================================================================

main :: IO ()
main = withSocketsDo $ 
  OCWS.runLocalClientWith
    WS.defaultConnectionOptions {
      WS.connectionStrictUnicode = True,
      WS.connectionFramePayloadSizeLimit = WS.NoSizeLimit
      -- WS.connectionFramePayloadSizeLimit = WS.SizeLimit 32
    }
    [] 
    app_demo

-- data WSMessage 
--   = PkgBoardStatus GameState
--   | PkgGameAction GameAction

sendEncodedPackage :: WS.Connection -> OCTypes.WSMessage -> IO()
sendEncodedPackage conn wsMsg = WS.sendBinaryData conn $ encode wsMsg
-- sendEncodedPackage conn wsMsg = WS.sendTextData conn $ encode wsMsg

app_demo :: WS.ClientApp ()
app_demo conn = do
    putStrLn "Connected!"

    sendEncodedPackage conn $ OCTypes.PkgGameAction OCTypes.ActRollDice
    sendEncodedPackage conn $ OCTypes.PkgGameAction (OCTypes.ActBuildRoad (OCTypes.EdgeId 10))
    sendEncodedPackage conn $ OCTypes.PkgGameAction (OCTypes.ActBuildCity (OCTypes.NodeId 20))
    sendEncodedPackage conn $ OCTypes.PkgGameAction (OCTypes.ActBuildSettlement (OCTypes.NodeId 30))
    sendEncodedPackage conn $ OCTypes.PkgGameAction OCTypes.ActEndTurn
    
    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop

    loop
    WS.sendClose conn ("Bye!" :: Text)