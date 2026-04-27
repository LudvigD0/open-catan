module Types where 
import Data.Map (Map, fromList)
import Data.UUID
import Data.Word

-- Storage
-- data DataTile = DataResourceTile (Maybe Resource) Token Robber

-- Board 
data Cord = Cord Int Int Int deriving (Show, Eq, Ord)
data Board = Board {tiles :: Map Cord Tile} deriving Show

-- Catan types 
data Color    = Red | Blue | Orange | White deriving (Show, Eq)
data Resource = Lumber | Ore | Grain | Brick | Wool deriving (Show, Eq)
data Road     = Road PlayerId deriving Show
data Building = Settlement PlayerId | City PlayerId deriving Show

-- Player
newtype PlayerId = PlayerId UUID deriving (Show, Eq)

data Player = Player
  { playerId :: PlayerId,
    points :: Int,
    buildings :: [Node],
    roads :: [Edge]
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

data Node = Node NodeId (Maybe Building) [Edge] [Tile] deriving Show

-- example Boards
exampleBoard1 = Board { tiles = fromList [(Cord 0 0 0, exampleTile11)] }
exampleTile11 = Tile 
  { tileId = 1,
    resource = Just Lumber,
    token = 1,
    robber = False,
    nodes = [exampleNode1]
  }
exampleTile12 = Tile 
  { tileId = 2,
    resource = Just Brick,
    token = 2,
    robber = False,
    nodes = [exampleNode1]
  }
exampleTile13 = Tile 
  { tileId = 3,
    resource = Just Grain,
    token = 3,
    robber = False,
    nodes = [exampleNode1]
  }

exampleNode1 = Node (NodeId 1) (Just exampleBuilding1) [] [exampleTile11, exampleTile12, exampleTile13]

exampleBuilding1 = Settlement (playerId examplePlayer1)

examplePlayer1 = Player 
  { playerId = PlayerId $ fromWords64 1 1,
    points = 1,
    buildings = [exampleNode1],
    roads = []
  }



exampleBoard2 = Board { tiles = fromList [(Cord 0 0 0, exampleTile21)] }
exampleTile21 = Tile 
  { tileId = 1,
    resource = Just Lumber,
    token = 1,
    robber = False,
    nodes = [exampleNode21, exampleNode22, exampleNode24]
  }
exampleTile22 = Tile 
  { tileId = 2,
    resource = Just Brick,
    token = 2,
    robber = False,
    nodes = [exampleNode21, exampleNode22, exampleNode23]
  }
exampleTile23 = Tile 
  { tileId = 3,
    resource = Just Ore,
    token = 2,
    robber = False,
    nodes = [exampleNode21, exampleNode23, exampleNode24]
  }
exampleTile24 = Tile 
  { tileId = 4,
    resource = Just Grain,
    token = 4,
    robber = False,
    nodes = [exampleNode22]
  }
exampleTile25 = Tile 
  { tileId = 5,
    resource = Just Wool,
    token = 5,
    robber = False,
    nodes = [exampleNode23]
  }
exampleTile26 = Tile 
  { tileId = 6,
    resource = Nothing,
    token = 8,
    robber = True,
    nodes = [exampleNode24]
  }

exampleNode21 = Node (NodeId 1) (Just exampleBuilding2) [exampleEdge21, exampleEdge22, exampleEdge23] [exampleTile21, exampleTile22, exampleTile23]
exampleNode22 = Node (NodeId 2) Nothing [exampleEdge21] [exampleTile21, exampleTile22, exampleTile24]
exampleNode23 = Node (NodeId 3) Nothing [exampleEdge21] [exampleTile22, exampleTile23, exampleTile25]
exampleNode24 = Node (NodeId 4) Nothing [exampleEdge21] [exampleTile21, exampleTile23, exampleTile26]

exampleEdge21 = Edge (EdgeId 1) (Just exampleRoad2) (exampleNode21, exampleNode22)
exampleEdge22 = Edge (EdgeId 2) Nothing (exampleNode21, exampleNode23)
exampleEdge23 = Edge (EdgeId 3) Nothing (exampleNode21, exampleNode24)

exampleBuilding2 = Settlement (playerId examplePlayer1)

exampleRoad2 = Road (playerId examplePlayer1)

examplePlayer2 = Player
  { playerId = PlayerId $ fromWords64 2 2,
    points = 1,
    buildings = [exampleNode21],
    roads = [exampleEdge21]
  }
