module Types where 
import Data.Map (Map)
import Data.UUID

-- Storage
-- data DataTile = DataResourceTile (Maybe Resource) Token Robber

-- Board 
data Cord = Cord Int Int Int deriving (Show, Eq, Ord)
data Board = Board {tiles :: Map Cord Tile} deriving Show

-- Catan types 
data Color    = Red | Blue | Orange | White deriving (Show, Eq)
data Resource = Wood | Stone | Wheat | Clay | Sheep deriving (Show, Eq)
data Road     = Road PlayerId deriving Show
data Building = Settlement PlayerId | City PlayerId deriving Show

-- Player
newtype PlayerId = PlayerId UUID deriving (Show, Eq)

data Player = Player
  { playerId :: PlayerId,
    points :: Int,
    buildings :: [Building],
    roads :: [Road]
  } deriving Show

-- Game 

data ServerState = ServerState
  { activeGames :: [GameState]
  }

data GameState = GameState
  { gameId      :: Int
  , board       :: Board
  , players     :: [(Color, Player)]
  , currentTurn :: Int              -- Index of player  
  , phase       :: TurnPhase       
  , dice        :: (Int, Int)
  } deriving Show

data TurnPhase = Roll | Build | Trade deriving Show

-- Graph Structure
data Tile = Tile
  { tileId :: Int
  , resource :: Maybe Resource
  , token :: Int
  , robber :: Bool
  , nodes :: [Node]
  } deriving Show

newtype EdgeId = EdgeId Int deriving Show
newtype NodeId = NodeId Int deriving Show

data Edge = Edge EdgeId (Maybe Road) (Node, Node) deriving Show

data Node = Node NodeId (Maybe Building) [Edge] [Edge] deriving Show