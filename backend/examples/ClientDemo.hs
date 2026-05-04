{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DeriveAnyClass #-}

module Main 
    ( main
    ) where

import           Data.Aeson
import           GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as LC8
import           Foobar

import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
--import           WebSocket  -- our module

-- !!!
-- Until I've worked out how cabal wants you to define local modules and imports

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

-- MAIN CONTENT ================================================================
-- =============================================================================


main :: IO ()
main = withSocketsDo $ 
  runLocalClientWith
    WS.defaultConnectionOptions {
      WS.connectionStrictUnicode = True,
      WS.connectionFramePayloadSizeLimit = WS.NoSizeLimit
      -- WS.connectionFramePayloadSizeLimit = WS.SizeLimit 32
    }
    [] 
    app_demo

app_demo :: WS.ClientApp ()
app_demo conn = do
    putStrLn "Connected!"

    WS.sendTextData conn $ encode (Person {name = "Joe", age = 12})
    -- WS.sendTextData conn (encode Bishop)
    -- WS.sendTextData conn (encode [Bishop, Knight])

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