import Data.Map (Map)

-- Storage
-- data DataTile = DataResourceTile (Maybe Resource) Token Robber

-- data GameState = GameState

data Cord = Cord Int Int Int

data Board = Board {tiles :: Map Cord GraphTile}

data Building = Settlement PlayerId | City PlayerId

data Bridge = PlayerId

data Player = Player
  { playerId :: Int,
    points :: Int,
    buildings :: [Building],
    bridges :: [Bridge]
  }

data DataTile = DataTile
  { resource :: Maybe Resource,
    token :: Int,
    robber :: Bool
  }

data DataEdge = DataEdge (Maybe Bridge)

data DataNode = DataNode (Maybe Building)

-- Graph Structure
data GraphTile = GraphTile
  { graphTileId :: Int,
    dataTile :: DataTile,
    nodes :: [GraphNode]
  }

data GraphEdge = GraphEdge DataEdge (GraphNode, GraphNode)

data GraphNode = GraphNode DataNode [GraphEdge] [GraphTile]