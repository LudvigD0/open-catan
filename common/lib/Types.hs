{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where 
import Data.Map (Map)
import Data.UUID.Types
import Data.Aeson
import GHC.Generics

-- Board  ======================================================================
data Cord = Cord Int Int Int 
  deriving (Show, Eq, Ord, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

data Board = Board
  { tiles :: Map Cord Tile
  , nodes :: Map NodeId Node
  , edges :: Map EdgeId Edge
  }
  deriving(Eq, Generic, ToJSON, FromJSON)


-- Catan types =================================================================
data Color 
  = Red 
  | Blue 
  | Orange 
  | White 
  deriving (Show, Eq, Ord, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

data Resource 
  = Lumber 
  | Ore 
  | Grain 
  | Brick 
  | Wool 
  deriving (Show, Eq, Ord, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

data Road     = Road PlayerId 
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Building 
  = Settlement PlayerId 
  | City PlayerId 
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Player ======================================================================
newtype PlayerId = PlayerId UUID deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Player = Player
  { playerId  :: PlayerId
  , points    :: Int
  , buildings :: [NodeId]
  , roads     :: [EdgeId]
  , resources :: Map Resource Int 
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Game  =======================================================================

data GameState = GameState
  { gameId      :: Int
  , board       :: Board
  , players     :: Map Color Player
  , currentTurn :: Color    
  , turnPhase   :: TurnPhase
  , dice        :: (Int, Int)
  }
  deriving(Eq, Generic, ToJSON, FromJSON) -- do we need "Eq"? For now, Yes

-- API  ================================================================
-- Updated for GameActions, added to GameState
data TurnPhase = Roll | Build | GameOver Color 
 deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- All actions when a game loop is initialized
data GameAction
  = ActRollDice
  | ActBuildRoad EdgeId
  | ActBuildSettlement NodeId
  | ActBuildCity NodeId
  | ActEndTurn
  deriving(Show, Eq, Generic, ToJSON, FromJSON)

--- Errors for the API 
data GameError
  = InvalidPhase TurnPhase GameAction
  | GameNotStarted
  | InvalidRoadPlacement
  | InvalidSettlementPlacement
  | InvalidCityPlacement
  | InvalidGameAction
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Socket Data  ================================================================
data WSMessage 
  = PkgBoardStatus GameState
  | PkgGameAction GameAction
  deriving(Eq, Generic, ToJSON, FromJSON) -- do we need "Eq"? -- For now, yes

-- Graph Structure  ============================================================
newtype TileId = TileId Int 
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

newtype EdgeId = EdgeId Int 
  deriving (Show, Eq, Ord, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

newtype NodeId = NodeId Int 
  deriving (Show, Eq, Ord, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

data Tile = Tile
  { tileId   :: TileId
  , resource :: Maybe Resource
  , token    :: Int
  , robber   :: Bool
  , tileNodes :: [NodeId]
  , tileEdges :: [EdgeId]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Edge = Edge
  { edgeId    :: EdgeId
  , road      :: Maybe Road
  , edgeNodes :: (NodeId, NodeId)
  } deriving (Eq, Generic, ToJSON, FromJSON)

data Node = Node
  { nodeId    :: NodeId
  , building  :: Maybe Building
  , nodeEdges :: [EdgeId]
  , nodeTiles :: [TileId]
  } deriving (Eq, Generic, ToJSON, FromJSON)


data ServerState = ServerState
  { activeGames :: [GameState]
  } deriving Eq
  

-------------- Viktor, jag flyttade Gameresponse då jag behöver den också

-- Respose which represent successful or failed request
data GameResponse
  = GameStateResponse GameState
  | GameErrorResponse GameError
  deriving (Eq, Generic)

instance FromJSON GameResponse
