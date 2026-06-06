{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Core domain and wire types shared by the frontend, backend, and game logic.
module Types
  ( Cord(..)
  , Board(..)
  , Color(..)
  , Resource(..)
  , Road(..)
  , Building(..)
  , PlayerId(..)
  , Player(..)
  , GameState(..)
  , TurnPhase(..)
  , GameAction(..)
  , GameError(..)
  , WSMessage(..)
  , TileId(..)
  , EdgeId(..)
  , NodeId(..)
  , Tile(..)
  , Edge(..)
  , Node(..)
  , GameResponse(..)
  ) where

import Data.Aeson
  ( FromJSON
  , FromJSONKey
  , ToJSON
  , ToJSONKey
  )
import Data.Map (Map)
import Data.UUID.Types (UUID)
import GHC.Generics (Generic)

-- * Board model

-- | Cube coordinate for a hex tile. Valid coordinates satisfy q + r + s == 0.
data Cord = Cord Int Int Int 
  deriving (Show, Eq, Ord, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

-- | Complete board graph, indexed by tile coordinates, node ids, and edge ids.
data Board = Board
  { tiles :: Map Cord Tile
  , nodes :: Map NodeId Node
  , edges :: Map EdgeId Edge
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

-- * Catan pieces and resources

-- | Player color used as the public key for turn order and player lookup.
data Color 
  = Red 
  | Blue 
  | Orange 
  | White 
  deriving (Show, Eq, Ord, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

-- | Resource cards produced by board tiles and spent on builds.
data Resource 
  = Lumber 
  | Ore 
  | Grain 
  | Brick 
  | Wool 
  deriving (Show, Eq, Ord, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

-- | A road placed on an edge by a player.
data Road     = Road PlayerId 
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A settlement or city placed on a node by a player.
data Building 
  = Settlement PlayerId 
  | City PlayerId 
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- * Player and game state

-- | Stable identifier for a player connection or account.
newtype PlayerId = PlayerId UUID deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Player-specific state tracked by the game engine.
data Player = Player
  { playerId  :: PlayerId
  , points    :: Int
  , buildings :: [NodeId]
  , roads     :: [EdgeId]
  , resources :: Map Resource Int 
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Complete state for one game instance.
data GameState = GameState
  { gameId      :: Int
  , board       :: Board
  , players     :: Map Color Player
  , currentTurn :: Color    
  , turnPhase   :: TurnPhase
  , dice        :: (Int, Int)
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

-- | High-level phase of the current turn.
data TurnPhase = Roll | Build | GameOver Color 
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Client-requested action to apply to the current game state.
data GameAction
  = ActRollDice
  | ActBuildRoad EdgeId
  | ActBuildSettlement NodeId
  | ActBuildCity NodeId
  | ActEndTurn
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Domain errors returned when a game action cannot be applied.
data GameError
  = InvalidPhase TurnPhase GameAction
  | GameNotStarted
  | InvalidRoadPlacement
  | InvalidSettlementPlacement
  | InvalidCityPlacement
  | InvalidGameAction
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- * Transport messages

-- | WebSocket message exchanged between clients and the server.
data WSMessage 
  = PkgBoardStatus GameState
  | PkgGameAction GameAction
  deriving (Eq, Generic, ToJSON, FromJSON)

-- * Board graph identifiers and nodes

-- | Identifier for a tile in the generated board graph.
newtype TileId = TileId Int 
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Identifier for an edge between two board nodes.
newtype EdgeId = EdgeId Int 
  deriving (Show, Eq, Ord, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

-- | Identifier for a node where buildings can be placed.
newtype NodeId = NodeId Int 
  deriving (Show, Eq, Ord, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

-- | Hex tile metadata and the graph elements that surround it.
data Tile = Tile
  { tileId   :: TileId
  , resource :: Maybe Resource
  , token    :: Int
  , robber   :: Bool
  , tileNodes :: [NodeId]
  , tileEdges :: [EdgeId]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Board edge that may contain a road.
data Edge = Edge
  { edgeId    :: EdgeId
  , road      :: Maybe Road
  , edgeNodes :: (NodeId, NodeId)
  } deriving (Eq, Generic, ToJSON, FromJSON)

-- | Board node that may contain a settlement or city.
data Node = Node
  { nodeId    :: NodeId
  , building  :: Maybe Building
  , nodeEdges :: [EdgeId]
  , nodeTiles :: [TileId]
  } deriving (Eq, Generic, ToJSON, FromJSON)

-- | API response wrapper for successful state updates and domain errors.
data GameResponse
  = GameStateResponse GameState
  | GameErrorResponse GameError
  deriving (Eq, Generic, ToJSON, FromJSON)
