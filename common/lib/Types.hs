{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where 
import Data.Map (Map)
import Data.UUID.Types
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

data GameState = GameState
  { gameId      :: Int
  , board       :: Board
  , players     :: Map Color Player
  , currentTurn :: Color    
  , turnPhase   :: TurnPhase
  , dice        :: (Int, Int)
  }
  deriving(Eq, Generic, Json.ToJSON, Json.FromJSON) -- do we need "Eq"?

-- API  ================================================================
-- Updated for GameActions, added to GameState
data TurnPhase = Roll | Build | GameOver Color 
 deriving (Show, Eq, Generic, Json.ToJSON, Json.FromJSON)

-- All actions when a game loop is initialized
data GameAction
  = ActRollDice
  | ActBuildRoad EdgeId
  | ActBuildSettlement NodeId
  | ActBuildCity NodeId
  | ActEndTurn
  deriving(Show, Eq, Generic, Json.ToJSON, Json.FromJSON)

--- Errors for the API 
data GameError
  = InvalidPhase TurnPhase GameAction
  | GameNotStarted
  | InvalidRoadPlacement
  | InvalidSettlementPlacement
  | InvalidCityPlacement
  | InvalidGameAction
  deriving (Show, Eq, Generic, Json.ToJSON, Json.FromJSON)

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