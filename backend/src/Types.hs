
-- Storage
-- data DataTile = DataResourceTile (Maybe Resource) Token Robber

-- data GameState = GameState 


data Cord = Cord Int Int Int

data Board = Board 
{
    Tiles :: Map Cord GraphTile
}

data Building = Settlement PlayerId | City PlayerId 

data Bridge = PlayerId 

data Player = Player 
{
    PlayerId  :: Int,
    Points    :: Int,
    Buildings :: [Building],
    Bridges     :: [Bridge]
}

data DataTile = DataTile 
{
    Resource :: Maybe Resource,
    Token :: Int,
    Robber :: Bool
}

data DataEdge = DataEdge (Maybe Bridge)

data DataNode = DataNode (Maybe Building)

-- Graph Structure
data GraphTile = GraphTile DataTile [GraphNode]

data GraphTile = GraphTile 
{
    GraphTileId :: Int,
    DataTile    :: DataTile,
    Nodes       :: [GraphNode]
}

data GraphEdge = GraphEdge DataEdge (GraphNode, GraphNode)

data GraphNode = GraphNode DataNode [GraphEdge] [GraphTile]