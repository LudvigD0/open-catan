module BuildPhase where 

-- Libs
import System.IO (hFlush, stdout)
import qualified Data.Map as Map 
import Data.Maybe 
import Text.Read (readMaybe)

-- Local
import Types
import Util

-- Uses playerInput to place a new settlement, checks res in buildPhase
-- Adds the settlement to board, player, and removes req resources
{- placeSettlementIO :: GameState -> Color -> IO GameState
placeSettlementIO gs color = do
    putStr "Enter NodeId to place settlement (0 for exit): "
    hFlush stdout
    input <- getLine
    case readMaybe input of
        Nothing -> do
            putStrLn "Please enter a valid NodeId, or 0 to exit."
            placeSettlementIO gs color
        Just 0 -> return gs
        Just int -> do
            let nid = NodeId int
                pid = playerId $ getPlayer color gs
            case lookupNode nid (board gs) of
                Nothing -> do
                    putStrLn "Invalid NodeId, please try again."
                    placeSettlementIO gs color
                Just node ->
                    if validStlmPlacement node pid (board gs)
                        then do
                            let newBoard   = placeSettlement nid pid (board gs)
                                newPlayers = addSettlement color node (players gs)
                            return gs { board = newBoard, players = newPlayers }
                        else do
                            putStrLn "No connecting road or there is an existing build, please try again."
                            placeSettlementIO gs color

-- Uses playerInput to place a new road, checks res in buildPhase
-- Adds the road to board, player, and removes req resources
-- TODO: Check for valid placement / edgeId
placeRoadIO :: GameState -> Color -> IO GameState
placeRoadIO gs color = do
    putStr "Enter EdgeId to place road: "
    hFlush stdout
    input <- getLine
    let eid  = EdgeId (read input)
        pid = playerId $ getPlayer color gs
        newBoard   = placeRoad eid pid (board gs)
        edge = fromJust $ lookupEdge eid newBoard
        newPlayers = addRoad color edge (players gs)
    return gs { board = newBoard, players = newPlayers }

-- Uses playerInput to place a new city, checks res in buildPhase
-- Adds the city to board, player, and removes req resources
-- TODO: Check for valid placement / nodeId / existing settlement
placeCityIO :: GameState -> Color -> IO GameState
placeCityIO gs color = do
    putStr "Enter NodeId to upgrade to City: "
    hFlush stdout
    input <- getLine
    let nid = NodeId (read input)
        pid = playerId $ getPlayer color gs
        newBoard = placeCity nid pid (board gs)
    return gs { board = newBoard } -}


------------------------------ Resource Checking ----------------------------------------

-- Checks if the given color (player) has enough resources for settlement
checkSettlementRes :: GameState -> Color -> Bool
checkSettlementRes gs color = all (>= 1) [nLumber, nGrain, nBrick, nWool]
  where
    res     = resources $ getPlayer color gs 
    nLumber = res Map.! Lumber
    nGrain  = res Map.! Grain
    nBrick  = res Map.! Brick
    nWool   = res Map.! Wool
    
-- Checks if the given color (player) has enough resources for road
checkRoadRes :: GameState -> Color -> Bool
checkRoadRes gs color = all (>= 1) [nLumber, nBrick]
  where
    res     = resources $ getPlayer color gs
    nLumber = res Map.! Lumber
    nBrick  = res Map.! Brick

-- Checks if the given color (player) has enough resources for city
checkCityRes :: GameState -> Color -> Bool
checkCityRes gs color = nOre >= 3 && nGrain >= 2
  where
    res     = resources $ getPlayer color gs
    nOre   = res Map.! Ore
    nGrain = res Map.! Grain

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

------------------------------ Validity Checks ----------------------------------------

-- Checks if player has settlement on node
validCityPlacement :: NodeId -> PlayerId -> Board -> Bool 
validCityPlacement nid pid brd | isNothing node = False 
                               | otherwise = case building (fromJust node) of 
                                            Just (Settlement nodepid) -> pid == nodepid
                                            _                         -> False 
 where 
    node = lookupNode nid brd

-- Checks if node is empty, checks all adjacent nodes within radius 1 
-- Checks for min 1 road connected to node
validStlmPlacement :: NodeId -> PlayerId -> Board -> Bool
validStlmPlacement nid pid brd | isNothing node = False
                               | otherwise = isNothing (building (fromJust node)) &&                              
                                all (isNothing . building) (adjacentNodes (fromJust node) brd) &&  
                                hasConnectingRoad (fromJust node) pid brd 
 where 
    node = lookupNode nid brd

-- Checks if player has a road connected to node 
hasConnectingRoad :: Node -> PlayerId -> Board -> Bool
hasConnectingRoad node pid brd = any isOwnRoad (nodeEdges node)  
  where
    isOwnRoad eid = case road (fromJust $ lookupEdge eid brd) of
        Just (Road ownerId) -> ownerId == pid
        Nothing             -> False