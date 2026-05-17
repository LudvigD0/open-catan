{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where 
import Data.Map (Map, fromList, toList, elems)
import Data.UUID.Types
import Data.Word
import Data.List (nub)
import Data.Tuple.Extra (both)
import qualified Data.Aeson as Json
import           GHC.Generics

-- Board  ======================================================================
data Cord = Cord Int Int Int 
  deriving (Show, Eq, Ord, Generic, Json.ToJSON, Json.ToJSONKey, Json.FromJSON, Json.FromJSONKey)


data Board = Board
  { tiles :: Map Cord Tile
  , nodes :: Map NodeId Node
  , edges :: Map EdgeId Edge
  }
  deriving(Eq, Generic, Json.ToJSON, Json.FromJSON)

data Action
  = NoOp
  | ClickHex
  | ClickNode NodeId
  deriving (Show, Eq)

-- Catan types =================================================================
data Color 
  = Red 
  | Blue 
  | Orange 
  | White 
  deriving (Show, Eq, Ord, Generic, Json.ToJSON, Json.ToJSONKey, Json.FromJSON, Json.FromJSONKey)

data Resource 
  = Lumber 
  | Ore 
  | Grain 
  | Brick 
  | Wool 
  deriving (Show, Eq, Ord, Generic, Json.ToJSON, Json.ToJSONKey, Json.FromJSON, Json.FromJSONKey)

data Road     = Road PlayerId 
  deriving (Show, Eq, Generic, Json.ToJSON, Json.FromJSON)

data Building 
  = Settlement PlayerId 
  | City PlayerId 
  deriving (Show, Eq, Generic, Json.ToJSON, Json.FromJSON)

-- Player ======================================================================
newtype PlayerId = PlayerId UUID deriving (Show, Eq, Generic, Json.ToJSON, Json.FromJSON)

data Player = Player
  { playerId  :: PlayerId
  , points    :: Int
  , buildings :: [NodeId]
  , roads     :: [EdgeId]
  , resources :: Map Resource Int 
  } deriving (Show, Eq, Generic, Json.ToJSON, Json.FromJSON)

-- Game  =======================================================================
data ServerState = ServerState
  { activeGames :: [GameState]
  }

data GameState = GameState
  { gameId      :: Int
  , board       :: Board
  , players     :: Map Color Player
  , currentTurn :: Color              -- Index of player  
  , dice        :: (Int, Int)
  }
  deriving(Eq, Generic, Json.ToJSON, Json.FromJSON) -- do we need "Eq"?


data TurnPhase = Roll | Build | Trade deriving (Show, Eq, Generic, Json.ToJSON, Json.FromJSON)

data GameAction 
  = ActNextPhase 
  | ActBuildRoad Road 
  | ActBuildBuilding Building
  deriving(Show, Eq, Generic, Json.ToJSON, Json.FromJSON)

-- Socket Data  ================================================================
data WSMessage 
  = PkgBoardStatus GameState
  | PkgGameAction GameAction
  deriving(Eq, Generic, Json.ToJSON, Json.FromJSON) -- do we need "Eq"?


-- Graph Structure  ============================================================
newtype TileId = TileId Int 
  deriving (Show, Eq, Ord, Generic, Json.ToJSON, Json.FromJSON)

newtype EdgeId = EdgeId Int 
  deriving (Show, Eq, Ord, Generic, Json.ToJSON, Json.ToJSONKey, Json.FromJSON, Json.FromJSONKey)

newtype NodeId = NodeId Int 
  deriving (Show, Eq, Ord, Generic, Json.ToJSON, Json.ToJSONKey, Json.FromJSON, Json.FromJSONKey)


data Tile = Tile
  { tileId   :: TileId
  , resource :: Maybe Resource
  , token    :: Int
  , robber   :: Bool
  , tileNodes :: [NodeId]
  , tileEdges :: [EdgeId]
  } deriving (Show, Eq, Generic, Json.ToJSON, Json.FromJSON)

data Edge = Edge
  { edgeId    :: EdgeId
  , road      :: Maybe Road
  , edgeNodes :: (NodeId, NodeId)
  } deriving (Eq, Generic, Json.ToJSON, Json.FromJSON)

data Node = Node
  { nodeId    :: NodeId
  , building  :: Maybe Building
  , nodeEdges :: [EdgeId]
  , nodeTiles :: [TileId]
  } deriving (Eq, Generic, Json.ToJSON, Json.FromJSON)

{-
instance Show Board where
  show = (++) " tiles: " . show . map tupleRepl . toList . tiles
    where
      tupleRepl (c, t) = (c, tileId t)

instance Show GameState where
  show gs = "id: " ++ show (gameId gs) ++
            " players: " ++ show (map shortPlayer (players gs)) ++ 
            " turn: " ++ show (currentTurn gs) ++ 
            " phase: " ++ show (phase gs) ++
            " dice: " ++ diceCase (dice gs)
    where
      shortPlayer (c, p) = (c, playerId p)
      diceCase dices = case dices of
        (0, 0)   -> "no Dices thrown"
        (d1, d2) -> show d1 ++ " " ++ show d2

instance Show Player where
  show p = "id: " ++ show (playerId p) ++
            " points: " ++ show (points p) ++ 
            " buildings: " ++ show (map nodeId (buildings p)) ++ 
            " bridges: " ++ show (map edgeId (roads p)) ++
            " cards: " ++ show (resourceCards p)

instance Show Node where
  show n = "id: " ++ show (nodeId n) ++
            " building: " ++ show (building n) ++ 
            " near edges: " ++ show (map edgeId (edges n)) ++ 
            " near tiles: " ++ show (map tileId (nodeTiles n))

instance Show Edge where
  show e = "id: " ++ show (edgeId e) ++
            " road: " ++ show (road e) ++ 
            " path: " ++ show (both nodeId (path e))


instance Show Tile where
  show t = "id: " ++ show (tileId t) ++
            " resource: " ++ show (resource t) ++ 
            " token: " ++ show (token t) ++ 
            " robber: " ++ show (robber t) ++ 
            " near nodes: " ++ show (map nodeId (nodes t))

-}

