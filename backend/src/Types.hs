module Types where

import Data.Map (Map, fromList)
import Test.QuickCheck (Arbitrary)

-- Storage
-- data DataTile = DataResourceTile (Maybe Resource) Token Robber

-- data GameState = GameState

data Cord = Cord Int Int Int
  deriving ( Eq )

data Board = Board {tiles :: Map Cord GraphTile}

data Building = Settlement Player | City Player

data Road = Road Player

data Player = Player
  { playerId :: Int,
    points :: Int,
    buildings :: [(Building, GraphNode)],
    bridges :: [(Road, GraphEdge)]
  }

data DataTile = DataTile
  { resource :: Maybe Resource,
    token :: Int,
    robber :: Bool
  }

data DataEdge = DataEdge { road :: Maybe Road }

data DataNode = DataNode { building :: Maybe Building }

-- Graph Structure
data GraphTile = GraphTile
  { graphTileId :: Int,
    dataTile :: DataTile,
    nodes :: [GraphNode]
  }

data GraphEdge = GraphEdge DataEdge (GraphNode, GraphNode)

data GraphNode = GraphNode DataNode [GraphEdge] [GraphTile]

data Resource = Wool | Grain | Wood | Brick | Rock
  deriving ( Show )

instance Ord Cord where
  (Cord a b c) <= (Cord d f g) = a < f || a <= d

-- example Boards
exampleBoard1 = Board { tiles = fromList [(Cord 0 0 0, exampleGraphTile11)] }
exampleGraphTile11 = GraphTile 
  { graphTileId = 1,
    dataTile = exampleDataTile11,
    nodes = [exampleGraphNode1]
  }
exampleGraphTile12 = GraphTile 
  { graphTileId = 2,
    dataTile = exampleDataTile12,
    nodes = [exampleGraphNode1]
  }
exampleGraphTile13 = GraphTile 
  { graphTileId = 3,
    dataTile = exampleDataTile13,
    nodes = [exampleGraphNode1]
  }

exampleDataTile11 = DataTile 
  { resource = Just Wood,
    token = 1,
    robber = False
  }
exampleDataTile12 = DataTile 
  { resource = Just Brick,
    token = 2,
    robber = False
  }
exampleDataTile13 = DataTile 
  { resource = Just Rock,
    token = 3,
    robber = False
  }

exampleGraphNode1 = GraphNode exampleDataNode1 [] [exampleGraphTile11, exampleGraphTile12, exampleGraphTile13]

exampleDataNode1 = DataNode { building = Just exampleBuilding1 }

exampleBuilding1 = Settlement examplePlayer1

examplePlayer1 = Player 
  { playerId = 1,
    points = 1,
    buildings = [(exampleBuilding1, exampleGraphNode1)],
    bridges = []
  }




exampleGraphNode21 = GraphNode exampleDataNode21 [exampleGraphEdge21, exampleGraphEdge22, exampleGraphEdge23] [exampleGraphTile21, exampleGraphTile22, exampleGraphTile23]
exampleGraphNode22 = GraphNode exampleDataNode22 [exampleGraphEdge21] [exampleGraphTile21, exampleGraphTile22, exampleGraphTile24]
exampleGraphNode23 = GraphNode exampleDataNode23 [exampleGraphEdge21] [exampleGraphTile22, exampleGraphTile23, exampleGraphTile25]
exampleGraphNode24 = GraphNode exampleDataNode24 [exampleGraphEdge21] [exampleGraphTile21, exampleGraphTile23, exampleGraphTile26]

exampleGraphEdge21 = GraphEdge exampleDataEdge21 (exampleGraphNode21, exampleGraphNode22)
exampleGraphEdge22 = GraphEdge exampleDataEdge22 (exampleGraphNode21, exampleGraphNode23)
exampleGraphEdge23 = GraphEdge exampleDataEdge23 (exampleGraphNode21, exampleGraphNode24)

exampleDataNode21 = DataNode { building = Just exampleBuilding2 }
exampleDataNode22 = DataNode { building = Nothing }
exampleDataNode23 = DataNode { building = Nothing }
exampleDataNode24 = DataNode { building = Nothing }

exampleDataEdge21 = DataEdge { road = Just exampleRoad2 }
exampleDataEdge22 = DataEdge { road = Nothing }
exampleDataEdge23 = DataEdge { road = Nothing }

exampleBoard2 = Board { tiles = fromList [(Cord 0 0 0, exampleGraphTile21)] }
exampleGraphTile21 = GraphTile 
  { graphTileId = 1,
    dataTile = exampleDataTile21,
    nodes = [exampleGraphNode21, exampleGraphNode22, exampleGraphNode24]
  }
exampleGraphTile22 = GraphTile 
  { graphTileId = 2,
    dataTile = exampleDataTile22,
    nodes = [exampleGraphNode21, exampleGraphNode22, exampleGraphNode23]
  }
exampleGraphTile23 = GraphTile 
  { graphTileId = 3,
    dataTile = exampleDataTile23,
    nodes = [exampleGraphNode21, exampleGraphNode23, exampleGraphNode24]
  }
exampleGraphTile24 = GraphTile 
  { graphTileId = 4,
    dataTile = exampleDataTile24,
    nodes = [exampleGraphNode22]
  }
exampleGraphTile25 = GraphTile 
  { graphTileId = 5,
    dataTile = exampleDataTile25,
    nodes = [exampleGraphNode23]
  }
exampleGraphTile26 = GraphTile 
  { graphTileId = 6,
    dataTile = exampleDataTile26,
    nodes = [exampleGraphNode24]
  }

exampleDataTile21 = DataTile 
  { resource = Just Wood,
    token = 1,
    robber = False
  }
exampleDataTile22 = DataTile 
  { resource = Just Brick,
    token = 2,
    robber = False
  }
exampleDataTile23 = DataTile 
  { resource = Just Rock,
    token = 3,
    robber = False
  }
exampleDataTile24 = DataTile 
  { resource = Just Grain,
    token = 4,
    robber = False
  }
exampleDataTile25 = DataTile 
  { resource = Just Wool,
    token = 5,
    robber = False
  }
exampleDataTile26 = DataTile 
  { resource = Nothing,
    token = 6,
    robber = True
  }

exampleBuilding2 = Settlement examplePlayer2

exampleRoad2 = Road examplePlayer2

examplePlayer2 = Player 
  { playerId = 1,
    points = 1,
    buildings = [(exampleBuilding2, exampleGraphNode21)],
    bridges = [(exampleRoad2, exampleGraphEdge21)]
  }