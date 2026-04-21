module Types where

import Data.Map (Map)

-- Storage
-- data DataTile = DataResourceTile (Maybe Resource) Token Robber

-- data GameState = GameState

data Cord = Cord Int Int Int

data Board = Board {tiles :: Map Cord GraphTile}

data Building = Settlement Player | City Player

data Road = PlayerId

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