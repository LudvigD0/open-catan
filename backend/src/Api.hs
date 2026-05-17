{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (elems, filter)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
import Data.UUID

-- Local
import Main
import Catan
import Types

-- ============================================================
-- 1. DATA TYPES
-- ============================================================

{- - GET response: a rectangle with its computed area
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

instance ToJSON AddResult -}

-- Request initalization of a new game
data InitGameRequest = InitGameRequest
  { initPlayerUUIDs :: [UUID]
  } deriving (Generic) -- deriving (Show, Generic)

instance FromJSON InitGameRequest

-- New initialized game state
data InitGameResponse = InitGameResponse
  { newGameState :: GameState
  } deriving (Generic) -- deriving (Show, Generic)

instance ToJSON InitGameResponse

-- Request to update game turn
data NextTurnRequest = NextTurnRequest
  { nextTurnInput :: GameState
  } deriving (Generic) -- deriving (Show, Generic)

instance FromJSON NextTurnRequest

-- Game state with updated game turn
data NextTurnResponse = NextTurnResponse
  { nextTurnOutput :: GameState
  } deriving (Generic) -- deriving (Show, Generic)

instance ToJSON NextTurnResponse

-- Request to throw dice
data ThrowDiceRequest = ThrowDiceRequest
  { throwDiceInput :: GameState
  , newDiceRoll       :: (Int, Int) -- Results for the dice rolls will be handled on the server
  } deriving (Generic) -- deriving (Show, Generic)

instance FromJSON ThrowDiceRequest

-- Game with dice thrown and resulting resource distribution
data ThrowDiceResponse = ThrowDiceResponse
  { throwDiceOutput :: GameState
  } deriving (Generic) -- deriving (Show, Generic)

instance ToJSON ThrowDiceResponse

-- Request to throw dice
data DistributeRequest = DistributeRequest
  { distributeInput :: GameState
  } deriving (Generic) -- deriving (Show, Generic)

instance FromJSON DistributeRequest

-- Game with dice thrown and resulting resource distribution
data DistributeResponse = DistributeResponse
  { distributeOutput :: GameState
  } deriving (Generic) -- deriving (Show, Generic)

instance ToJSON DistributeResponse

-- Request to build settlement
data SettlementRequest = SettlementRequest
  { settlementInput :: GameState
  , settlementNode :: NodeId
  } deriving (Generic) -- deriving (Show, Generic)

instance FromJSON SettlementRequest

-- Game with new settlement (if building it is allowed)
data SettlementResponse = SettlementResponse
  { settlementOutput :: GameState
  } deriving (Generic) -- deriving (Show, Generic)

instance ToJSON SettlementResponse

-- Request to build city
data CityRequest = CityRequest
  { cityInput :: GameState
  , cityNode :: NodeId
  } deriving (Generic) -- deriving (Show, Generic)

instance FromJSON CityRequest

-- Game with new city (if building it is allowed)
data CityResponse = CityResponse
  { cityOutput :: GameState
  } deriving (Generic) -- deriving (Show, Generic)

instance ToJSON CityResponse

-- Request to build road
data RoadRequest = RoadRequest
  { roadInput :: GameState
  , roadEdge :: EdgeId
  } deriving (Generic) -- deriving (Show, Generic)

instance FromJSON RoadRequest

-- Game with new road (if building it is allowed)
data RoadResponse = RoadResponse
  { roadOutput :: GameState
  } deriving (Generic) -- deriving (Show, Generic)

instance ToJSON RoadResponse

-- Request from player to declare victory
data WinRequest = WinRequest
  { winInput :: GameState
  , winningPlayer :: PlayerId
  } deriving (Generic) -- deriving (Show, Generic)

instance FromJSON WinRequest

-- Respond with true if the player is allowed to declare victory
data WinResponse = WinResponse
  { playerWins :: Bool
  } deriving (Generic) -- deriving (Show, Generic)

instance ToJSON WinResponse



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
{-       "rectangle" :> QueryParam "width"  Double
                   :> QueryParam "height" Double
                   :> Get '[JSON] Rectangle
  :<|> "add"       :> ReqBody '[JSON] AddRequest
                   :> Post    '[JSON] AddResult -}
       "init"      :> ReqBody '[JSON] InitGameRequest
                   :> Post    '[JSON] InitGameResponse
  :<|> "next"      :> ReqBody '[JSON] NextTurnRequest
                   :> Post    '[JSON] NextTurnResponse
  :<|> "dice"      :> ReqBody '[JSON] ThrowDiceRequest
                   :> Post    '[JSON] ThrowDiceResponse
  :<|> "dist"      :> ReqBody '[JSON] DistributeRequest
                   :> Post    '[JSON] DistributeResponse
  :<|> "sett"      :> ReqBody '[JSON] SettlementRequest
                   :> Post    '[JSON] SettlementResponse
  :<|> "city"      :> ReqBody '[JSON] CityRequest
                   :> Post    '[JSON] CityResponse
  :<|> "road"      :> ReqBody '[JSON] RoadRequest
                   :> Post    '[JSON] RoadResponse
  :<|> "win"       :> ReqBody '[JSON] WinRequest
                   :> Post    '[JSON] WinResponse


-- ============================================================
-- 4. HANDLERS  (one per route, in the same order as the API type)
-- ============================================================

{-
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

-}

initGameHandler :: InitGameRequest -> Handler InitGameResponse
initGameHandler req = do 
  let uuids = initPlayerUUIDs req
  return $ InitGameResponse $ initGameState uuids 0
  
nextTurnHandler :: NextTurnRequest -> Handler NextTurnResponse
nextTurnHandler req = do 
  let gs = nextTurnInput req
  return $ NextTurnResponse $ gs { currentTurn = nextPlayer (currentTurn gs) (players gs) }

throwDiceHandler :: ThrowDiceRequest -> Handler ThrowDiceResponse
throwDiceHandler req = do 
  let gs = throwDiceInput req
      roll = newDiceRoll req
  return $ ThrowDiceResponse $ gs { dice = roll }

distributeHandler :: DistributeRequest -> Handler DistributeResponse
distributeHandler req = do 
  let gs = distributeInput req
      roll = uncurry (+) $ dice gs
      newPlayers = distributeResources roll (board gs) (players gs)
  return $ DistributeResponse $ gs { players = newPlayers }

settlementHandler :: SettlementRequest -> Handler SettlementResponse
settlementHandler req = do
  let gs = settlementInput req
      node = settlementNode req
      player = getPlayer gs $ currentTurn gs
      newBoard = placeSettlement node (playerId player) (board gs)
  return $ SettlementResponse $ gs { board = newBoard }

cityHandler :: CityRequest -> Handler CityResponse
cityHandler req = do
  let gs = cityInput req
      node = cityNode req
      player = getPlayer gs $ currentTurn gs
      newBoard = placeCity node (playerId player) (board gs)
  return $ CityResponse $ gs { board = newBoard }


roadHandler :: RoadRequest -> Handler RoadResponse
roadHandler req = do
  let gs = roadInput req
      node = roadEdge req
      player = getPlayer gs $ currentTurn gs
      newBoard = placeRoad node (playerId player) (board gs)
  return $ RoadResponse $ gs { board = newBoard }

winHandler :: WinRequest -> Handler WinResponse
winHandler req = do
  let gs = winInput req
      plrId  = winningPlayer req
      player = elems $ Data.Map.filter ((plrId==) . playerId) (players gs)
  return $ case player of
    []   -> WinResponse False
    x:_ -> WinResponse (countVP x (board gs) >= 10)


-- ============================================================
-- 5. WIRING  (connect the API type to the handlers)
-- ============================================================

server :: Server API
server = 
       initGameHandler 
  :<|> nextTurnHandler 
  :<|> throwDiceHandler
  :<|> distributeHandler 
  :<|> settlementHandler
  :<|> cityHandler 
  :<|> roadHandler
  :<|> winHandler

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