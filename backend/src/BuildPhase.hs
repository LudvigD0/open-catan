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
placeSettlementIO :: GameState -> Color -> IO GameState
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
    return gs { board = newBoard }


------------------------------ Resource Checking ----------------------------------------

-- Checks if the given color (player) has enough resources for settlement
checkSettlementRes :: GameState -> Color -> Bool
checkSettlementRes gs color = all (>= 1) [nLumber, nGrain, nBrick, nWool]
  where
    res     = resources . snd . head $ filter ((== color) . fst) (players gs)
    nLumber = res Map.! Lumber
    nGrain  = res Map.! Grain
    nBrick  = res Map.! Brick
    nWool   = res Map.! Wool
    
-- Checks if the given color (player) has enough resources for road
checkRoadRes :: GameState -> Color -> Bool
checkRoadRes gs color = all (>= 1) [nLumber, nBrick]
  where
    res     = resources . snd . head $ filter ((== color) . fst) (players gs)
    nLumber = res Map.! Lumber
    nBrick  = res Map.! Brick

-- Checks if the given color (player) has enough resources for city
checkCityRes :: GameState -> Color -> Bool
checkCityRes gs color = nOre >= 3 && nGrain >= 2
  where
    res    = resources . snd . head $ filter ((== color) . fst) (players gs)
    nOre   = res Map.! Ore
    nGrain = res Map.! Grain

------------------------------ Board State ----------------------------------------

-- Adds new settlement to the board given a nodeId and playerId
placeSettlement :: NodeId -> PlayerId -> Board -> Board
placeSettlement nid pid (Board tileMap) = Board $ Map.map updateTile tileMap
  where
    updateTile tile = tile { nodes = map updateNode (nodes tile) }
    updateNode n
        | nodeId n == nid = n { building = Just (Settlement pid) }
        | otherwise       = n

-- Adds new city to the board given a nodeId and playerId
placeCity :: NodeId -> PlayerId -> Board -> Board
placeCity nid pid (Board tileMap) = Board $ Map.map updateTile tileMap
  where
    updateTile tile = tile { nodes = map updateNode (nodes tile) }
    updateNode n
        | nodeId n == nid = n { building = Just (City pid) }
        | otherwise       = n

-- Adds new road to the board given a edgeId and playerId
placeRoad :: EdgeId -> PlayerId -> Board -> Board
placeRoad eid pid (Board tileMap) = Board $ Map.map updateTile tileMap
  where
    updateTile tile = tile { edges = map updateEdge (edges tile) }
    updateEdge e
        | edgeId e == eid = e { road = Just (Road pid) }
        | otherwise       = e

------------------------------ Player State ----------------------------------------

-- Add the city to the player and removes resources
addCity :: Color -> Node -> [(Color, Player)] -> [(Color, Player)]
addCity color node = map update
  where
    update (c, p)
        | c == color = (c, p { buildings = node : buildings p
                              , resources = updateRes (resources p) })
        | otherwise  = (c, p)
    updateRes resM = Map.mapWithKey deduct resM
    deduct res i
        | res == Grain = i - 2
        | res == Ore   = i - 3
        | otherwise    = i


-- Adds the settlement to the player, and remove the required resources 
addSettlement :: Color -> Node -> [(Color, Player)] -> [(Color, Player)]
addSettlement color node = map update
  where
    update (c, p)
        | c == color = (c, p { buildings = node : buildings p
                              , resources = updateRes (resources p) })
        | otherwise  = (c, p)
    updateRes resM = Map.mapWithKey deduct resM
    deduct res i
        | res `elem` [Lumber, Grain, Brick, Wool] = i - 1
        | otherwise                               = i

-- Adds the road to the player, and remove the required resources 
addRoad :: Color -> Edge -> [(Color, Player)] -> [(Color, Player)]  
addRoad color edge = map update
  where
    update (c, p)
        | c == color = (c, p { roads = edge : roads p
                             , resources = updateRes (resources p) })
        | otherwise  = (c, p)
    updateRes resM = Map.mapWithKey deduct resM
    deduct res i
        | res `elem` [Lumber, Brick] = i - 1
        | otherwise                  = i

-- Does not remove resources
addSettlementForced :: Color -> Node -> [(Color, Player)] -> [(Color, Player)]
addSettlementForced color node = map update
  where
    update (c, p)
        | c == color = (c, p { buildings = node : buildings p })
        | otherwise  = (c, p)

-- Does not remove resources
addRoadForced :: Color -> Edge -> [(Color, Player)] -> [(Color, Player)]  
addRoadForced color edge = map update
  where
    update (c, p)
        | c == color = (c, p { roads = edge : roads p })
        | otherwise  = (c, p)

------------------------------ Validity Checks ----------------------------------------

-- Checks if player has settlement on node
validCityPlacement :: Node -> PlayerId -> Board -> Bool 
validCityPlacement node pid1 board = hasBuilding
 where 
    hasBuilding = case building node of 
        Just (Settlement pid2) -> pid1 == pid2
        _                 -> False

-- Check if node is empty, no adjacent settlements within 1 road 
-- And player has at least one road leading to the node
validStlmPlacement :: Node -> PlayerId -> Board -> Bool
validStlmPlacement node pid board =
    isNothing (building node) &&                              
    all (isNothing . building) (adjacentNodes node board) &&  
    hasConnectingRoad node pid                                

hasConnectingRoad :: Node -> PlayerId -> Bool
hasConnectingRoad node pid = any isOwnRoad (nodeEdges node)  
  where
    isOwnRoad edge = case road edge of
        Just (Road ownerId) -> ownerId == pid
        Nothing             -> False