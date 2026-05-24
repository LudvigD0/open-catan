module Catan where 

-- Libs
import qualified Data.Map as Map 
import Data.Maybe (maybeToList)
import Data.UUID.Types

-- Local
import Coordinates
import Types

initGameState :: [UUID] -> Int -> GameState
initGameState ids start =
    let newPlayers                = map initPlayer ids 
        colors = [Red, Blue, Orange, White]
    in GameState
        { gameId      = 0
        , board       = catanBoard
        , players     = Map.fromList $ zip colors newPlayers
        , currentTurn = colors !! start 
        , turnPhase   = Roll
        , dice        = (0, 0)
        }

initPlayer :: UUID -> Player
initPlayer uid = Player
    { playerId  = PlayerId uid
    , points    = 0
    , buildings = []
    , roads   = []
    , resources = Map.fromList [(res, 0) | res <- [Lumber, Ore, Grain, Brick, Wool]]
    }

-- Uses beginnerplacements to add the starting settlements and road for each player
autoPlace :: GameState -> GameState
autoPlace gs = foldl placePair gs allPlacements
  where
    allPlacements =
        [ (color, nid, eid)
        | (color, pairs) <- beginnerPlacements, (nid, eid) <- pairs]


-- Adds a settlement and road at the same time 
placePair :: GameState -> (Color, NodeId, EdgeId) -> GameState
placePair gs (color, nid, eid) =
    let pid        = playerId $ (players gs) Map.! color
        newBoard   = placeRoad eid pid $ placeSettlement nid pid (board gs)
        newPlayers = addRoadForced color eid $ addSettlementForced color nid (players gs)
    in  gs { board = newBoard, players = newPlayers }


-- Each tuple: (settlement NodeId, road EdgeId)
beginnerPlacements :: [(Color, [(NodeId, EdgeId)])]
beginnerPlacements =
    [ ( Red
      , [ (NodeId 9, EdgeId 14)   
        , (NodeId 29, EdgeId 42)   
        ] )
    , ( Blue
      , [ (NodeId 42, EdgeId 53)   
        , (NodeId 40, EdgeId 57)   
        ] )
    , ( Orange
      , [ (NodeId 41, EdgeId 59)   
        , (NodeId 15, EdgeId 16)   
        ] )
    , ( White
      , [ (NodeId 18, EdgeId 26)   
        , (NodeId 32, EdgeId 38)   
        ] )
    ]

------------------------------ Util functions ----------------------------------------
-- Get a node from the board by NodeId
lookupNode :: NodeId -> Board -> Maybe Node
lookupNode nid brd = Map.lookup nid (nodes brd)

-- Get an edge from the board by EdgeId
lookupEdge :: EdgeId -> Board -> Maybe Edge
lookupEdge eid brd = Map.lookup eid (edges brd)

lookupTile :: Cord -> Board -> Maybe Tile 
lookupTile crd brd = Map.lookup crd (tiles brd)


-- Get a player by color
getPlayer :: Color -> GameState -> Player
getPlayer color gs = (players gs) Map.! color 

adjacentNodes :: Node -> Board -> [Node]
adjacentNodes node brd =
    concatMap edgeNeighbors (nodeEdges node)
  where
    edgeNeighbors eid =
        case Map.lookup eid (edges brd) of
            Nothing -> []
            Just edge ->
                let (n1, n2) = edgeNodes edge
                    other
                        | n1 == nodeId node = Just n2
                        | n2 == nodeId node = Just n1
                        | otherwise         = Nothing
                in maybe [] (\nid -> maybeToList $ lookupNode nid brd) other

------------------------------ Board State ----------------------------------------
-- Updates a board node with a new settlement belonging to playerid
placeSettlement :: NodeId -> PlayerId -> Board -> Board
placeSettlement nid pid brd =
    brd { nodes = Map.adjust updateNode nid (nodes brd) }
  where
    updateNode n = n { building = Just (Settlement pid) }

-- Updates a board node with a new city belonging to playerid
placeCity :: NodeId -> PlayerId -> Board -> Board
placeCity nid pid brd =
    brd { nodes = Map.adjust updateNode nid (nodes brd) }
  where
    updateNode n = n { building = Just (City pid) }

-- Updates a board edge with a new road belonging to playerid
placeRoad :: EdgeId -> PlayerId -> Board -> Board
placeRoad eid pid brd =
    brd { edges = Map.adjust updateEdge eid (edges brd) }
  where
    updateEdge e = e { road = Just (Road pid) }

------------------------------ Player State ----------------------------------------

-- Does not add duplicate nodeId since player needs to have settlement
-- to build city. Deducts resources for city 
addCity :: Color -> Map.Map Color Player -> Map.Map Color Player
addCity color = Map.adjust updatePlayer color
  where
    updatePlayer p =
        p { resources = updateResources (resources p) }

    updateResources =
        Map.mapWithKey deduct

    deduct res amount
        | res == Grain = amount - 2
        | res == Ore   = amount - 3
        | otherwise    = amount

-- Adds nodeId to players buildings and deducts resources for settlement 
addSettlement :: Color -> NodeId -> Map.Map Color Player -> Map.Map Color Player
addSettlement color nid plyrs =
    Map.adjust updatePlayer color plyrs
  where
    updatePlayer p =
        p
          { buildings = nid : buildings p
          , resources = updateRes (resources p)
          }

    updateRes resM = Map.mapWithKey deduct resM
    deduct res i
        | res `elem` [Lumber, Grain, Brick, Wool] = i - 1
        | otherwise                               = i

-- Adds edgeId to players roads, deducts resources for road
addRoad :: Color -> EdgeId -> Map.Map Color Player -> Map.Map Color Player
addRoad color eid plyrs =
    Map.adjust updatePlayer color plyrs
  where
    updatePlayer p =
        p
          { roads = eid : roads p
          , resources = updateRes (resources p)
          }

    updateRes resM = Map.mapWithKey deduct resM
    deduct res i
        | res `elem` [Lumber, Brick] = i - 1
        | otherwise                               = i
    
-- Does not remove resources
addSettlementForced :: Color -> NodeId -> Map.Map Color Player -> Map.Map Color Player
addSettlementForced color nid plyrs =
    Map.adjust updatePlayer color plyrs
  where
    updatePlayer p = p { buildings = nid : buildings p }

-- Does not remove resources
addRoadForced :: Color -> EdgeId -> Map.Map Color Player -> Map.Map Color Player
addRoadForced color eid plyrs =
    Map.adjust updatePlayer color plyrs
  where
    updatePlayer p = p { roads = eid : roads p }  
