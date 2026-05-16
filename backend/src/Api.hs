{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant

-- ============================================================
-- 1. DATA TYPES
-- ============================================================

-- GET response: a rectangle with its computed area
data Rectangle = Rectangle
  { width  :: Double
  , height :: Double
  , area   :: Double       -- computed by our logic function
  } deriving (Show, Generic)

instance ToJSON Rectangle

-- POST request body: two numbers to add
data AddRequest = AddRequest
  { numA :: Double
  , numB :: Double
  } deriving (Show, Generic)

instance FromJSON AddRequest

-- POST response: the result
data AddResult = AddResult
  { inputA :: Double
  , inputB :: Double
  , result :: Double       -- computed by our logic function
  } deriving (Show, Generic)

instance ToJSON AddResult


-- ============================================================
-- 2. BUSINESS LOGIC (pure functions, separate from HTTP layer)
-- ============================================================

computeArea :: Double -> Double -> Double
computeArea w h = w * h

computeSum :: Double -> Double -> Double
computeSum a b = a + b


-- ============================================================
-- 3. API TYPE  (the servant "spec" — routes live here)
-- ============================================================
--
--   GET  /rectangle?width=3&height=4   →  Rectangle JSON
--   POST /add                          →  AddResult JSON
--                                         (body: { "numA": 1, "numB": 2 })

type API =
       "rectangle" :> QueryParam "width"  Double
                   :> QueryParam "height" Double
                   :> Get '[JSON] Rectangle
  :<|> "add"       :> ReqBody '[JSON] AddRequest
                   :> Post    '[JSON] AddResult

  :<|> "action"    :> ReqBody '[JSON] GameAction
                   :> Post    '[JSON] GameState


-- ============================================================
-- 4. HANDLERS  (one per route, in the same order as the API type)
-- ============================================================

-- GET request 
rectangleHandler :: Maybe Double -> Maybe Double -> Handler Rectangle
rectangleHandler mw mh = do
  let w = maybe 1.0 id mw   -- default to 1.0 if param missing
      h = maybe 1.0 id mh
  return $ Rectangle w h (computeArea w h)

-- POST request 
addHandler :: AddRequest -> Handler AddResult
addHandler req = do
  let a = numA req
      b = numB req
  return $ AddResult a b (computeSum a b)


-- ============================================================
-- 5. WIRING  (connect the API type to the handlers)
-- ============================================================

server :: Server API
server = rectangleHandler :<|> addHandler

api :: Proxy API
api = Proxy

app :: Application
app = serve api server


-- ============================================================
-- 6. MAIN
-- ============================================================

main :: IO ()
main = do
  putStrLn "Running on http://localhost:8080"
  putStrLn "  GET  /rectangle?width=5&height=3"
  putStrLn "  POST /add          body: {\"numA\":10,\"numB\":32}"
  run 8080 app