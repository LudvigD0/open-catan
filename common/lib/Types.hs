module Types where 
import Data.Map (Map, fromList, toList, elems)
import Data.UUID.Types
import Data.Word
import Data.List (nub)
import Data.Tuple.Extra (both)

-- Board 
data Cord = Cord Int Int Int deriving (Show, Eq, Ord)
data Board = Board {tiles :: Map Cord Tile}

-- Catan types 
data Color    = Red | Blue | Orange | White deriving (Show, Eq)
data Resource = Lumber | Ore | Grain | Brick | Wool deriving (Show, Eq, Ord)
data Road     = Road PlayerId deriving (Show, Eq)
data Building = Settlement PlayerId | City PlayerId deriving (Show, Eq)

-- Player
newtype PlayerId = PlayerId UUID deriving (Show, Eq)

data Player = Player
  { playerId  :: PlayerId
  , points    :: Int
  , buildings :: [Node]
  , roads     :: [Edge]
  , resources :: Map Resource Int 
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
  , dice        :: (Int, Int)
  }

-- Graph Structure
newtype TileId = TileId Int deriving (Show, Eq, Ord)
newtype EdgeId = EdgeId Int deriving (Show, Eq, Ord)
newtype NodeId = NodeId Int deriving (Show, Eq, Ord)

data Tile = Tile
  { tileId   :: Int
  , resource :: Maybe Resource
  , token    :: Int
  , robber   :: Bool
  , nodes    :: [Node]
  , edges    :: [Edge]
  } deriving Show

data Edge = Edge
  { edgeId  :: EdgeId
  , road    :: Maybe Road
  , edgeNodes :: (Node, Node)
  } deriving Show

data Node = Node
  { nodeId    :: NodeId
  , building  :: Maybe Building
  , nodeEdges :: [Edge]
  , nodeTiles :: [Int]    -- stores tileId instead of Tile to break cyclical referensing
  } deriving Show

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

