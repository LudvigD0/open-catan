module Main where 

import Data.Maybe 
import qualified Data.Map as Map 
import Types 
import Coordinates
import Catan 
import System.IO (hFlush, stdout)
import Control.Monad
import Data.UUID
import Util
import System.Random

main :: IO ()
main = do
    gs <- initGameState
    let gs' = autoPlace gs
    putStrLn "Starting Catan!"
    gameLoop gs'

-- 1. game loop and rules
gameLoop :: GameState -> IO ()
gameLoop gs = do
    printBoard (board gs)
    let (color, player) = players gs !! currentTurn gs
    putStrLn $ "\n====== " ++ show color ++ "'s turn ======"

    -- Roll dice
    (d1, d2) <- rollDice
    let roll = d1 + d2
    putStrLn $ "Rolled: " ++ show roll
    -- Update gamestate
    let gs1 = gs { dice = (d1, d2) }

    -- Distribute resources
    let newPlayers = distributeResources roll (board gs1) (players gs1)
        gs2 = gs1 { players = newPlayers }
    putStrLn "Resources distributed."

    -- Build phase
    gs3 <- buildPhase gs2 color

    -- Step 8: Check VP
    case checkWinner gs3 of
        Just winner -> putStrLn $ "\n🎉 " ++ show winner ++ " wins!"
        Nothing     -> do
            -- Step 9: Next player
            let nPlayers  = length (players gs3)
                nextTurnIdx = (currentTurn gs3 + 1) `mod` nPlayers
                gs4 = gs3 { currentTurn = nextTurnIdx }
            gameLoop gs4

------------------------------------------------ Initaialization ------------------------------------------------------------------------------
-- Current issue, starting settlements and road are added to board but not player
initGameState :: IO GameState
initGameState = do
    let newPlayers                = map initPlayer someUUIDs    -- from Util
    return GameState
        { gameId      = 0
        , board       = catanBoard
        , players     = zip [Red, Blue, Orange, White] newPlayers
        , currentTurn = 0
        , phase       = Roll
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

-- Uses beginnerplacements to add the starting settlements and road for each player
autoPlace :: GameState -> GameState
autoPlace gs = foldl placePair gs allPlacements
  where
    allPlacements =
        [ (color, nid, eid)
        | (color, pairs) <- beginnerPlacements, (nid, eid) <- pairs]

-- Adds a settlement and road at the same time 
placePair :: GameState -> (Color, NodeId, EdgeId) -> GameState
placePair state (color, nid, eid) =
    let pid      = playerId . snd . head $ filter ((== color) . fst) (players state)
        newBoard = placeRoad eid pid $ placeSettlement nid pid (board state)
    in  state { board = newBoard }

------------------------------------------------VP Check------------------------------------------------------------------------------

-- Goes through every node in each tile and checks if building is 
-- present, then checks if it belongs to the given player. 
-- Calculates sum of points from only settlements and cities
-- TODO: Add Longest Road and largest army 
countVP :: Player -> Board -> Int
countVP p (Board tileMap) =
    sum [ vpFor b
        | tile <- Map.elems tileMap
        , node <- nodes tile
        , Just b <- [building node]
        , ownsBuilding (playerId p) b
        ]
  where
    vpFor (Settlement _) = 1
    vpFor (City _)       = 2
    ownsBuilding pid (Settlement p) = p == pid
    ownsBuilding pid (City p)       = p == pid

-- Goes through the current GameState, if a winner is found the other players are ignored 
-- Returns the color of the winning player 
checkWinner :: GameState -> Maybe Color
checkWinner gs =
    foldl check Nothing (players gs)
  where
    check (Just winner) _      = Just winner
    check Nothing (color, p)
        | countVP p (board gs) >= 10 = Just color
        | otherwise                   = Nothing

------------------------------------------------Building Phase------------------------------------------------------------------------------
printResources :: Player -> IO ()
printResources p = do
    putStrLn "\nYour resources:"
    mapM_ printRes (Map.toList (resources p))
  where
    printRes (res, n) = putStrLn $ "  " ++ show res ++ ": " ++ show n

printBoard :: Board -> IO ()
printBoard (Board tileMap) = do
    putStrLn "\n--- Board State ---"
    mapM_ printTile (Map.elems tileMap)
  where
    printTile t = putStrLn $
        "Tile " ++ show (tileId t) ++
        " [" ++ maybe "Desert" show (resource t) ++ "]" ++
        " token=" ++ show (token t)

-- Checks if the player can place items on the board by looking at resources,
-- also updates resources on placement
-- TODO: Check for valid placement, ex. roads connected to settlement, no house within 2 edges
buildPhase :: GameState -> Color -> IO GameState
buildPhase gs color = do
    let (_, player) = head $ filter ((== color) . fst) (players gs)
    printResources player
    putStrLn "\nBuild phase — choose an action:"
    when (checkSettlementRes gs color) $ putStrLn "  1. Place Settlement"
    when (checkRoadRes gs color)       $ putStrLn "  2. Place Road"
    when (checkCityRes gs color)       $ putStrLn "  3. Place City"
    putStrLn "  4. Skip"
    putStr "Choice: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" | checkSettlementRes gs color -> placeSettlementIO gs color
        "2" | checkRoadRes gs color       -> placeRoadIO gs color
        "3" | checkCityRes gs color       -> placeCityIO gs color
        "4"                               -> return gs
        _   -> do
            putStrLn "Invalid choice or insufficient resources."
            return gs

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

-- Uses playerInput to place a new settlement, checks res in buildPhase
-- Adds the settlement to board, player, and removes req resources
-- TODO: Check for valid placement / nodeId 
placeSettlementIO :: GameState -> Color -> IO GameState
placeSettlementIO gs color = do
    putStr "Enter NodeId to place settlement: "
    hFlush stdout
    input <- getLine
    let nid = NodeId (read input)
        pid = playerId . snd . head $ filter ((== color) . fst) (players gs)
        newBoard   = placeSettlement nid pid (board gs)
        node = fromJust $ lookupNode nid newBoard
        newPlayers = addSettlement color node (players gs)
    return gs { board = newBoard, players = newPlayers }

-- Uses playerInput to place a new road, checks res in buildPhase
-- Adds the road to board, player, and removes req resources
-- TODO: Check for valid placement / edgeId
placeRoadIO :: GameState -> Color -> IO GameState
placeRoadIO gs color = do
    putStr "Enter EdgeId to place road: "
    hFlush stdout
    input <- getLine
    let eid  = EdgeId (read input)
        pid  = playerId . snd . head $ filter ((== color) . fst) (players gs)
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
        pid = playerId . snd . head $ filter ((== color) . fst) (players gs)
        newBoard = placeCity nid pid (board gs)
    return gs { board = newBoard }

-- Adds the settlement to the player, and remove the required resourses 
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

-- Adds the road to the player, and remove the required resourses 
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

------------------------------------------------Distributing Resources------------------------------------------------------------------------------
-- Returns 2 ints representing dice 
rollDice :: IO (Int, Int)
rollDice = do
    d1 <- randomRIO (1, 6)
    d2 <- randomRIO (1, 6)
    return (d1, d2)

-- Find all tiles that match the dice roll token
matchingTiles :: Int -> Board -> [Tile]
matchingTiles roll (Board tileMap) =
    [ tile | tile <- Map.elems tileMap,
      token tile == roll,
      not (robber tile),
      resource tile /= Nothing]

-- Given a tile, find which players have settlements/cities on it
-- and what resource they should receive
tileYield :: Tile -> [(PlayerId, Resource, Int)]
tileYield tile = case resource tile of
    Nothing  -> []
    Just res ->
        [ (pid, res, amount)
        | node <- nodes tile
        , isJust (building node)
        , let b            = fromJust (building node)
              (pid, amount) = case b of
                  Settlement p -> (p, 1)
                  City p       -> (p, 2)
        ]

-- Distribute resources to all players based on dice roll
distributeResources :: Int -> Board -> [(Color, Player)] -> [(Color, Player)]
distributeResources roll board ps =
    foldl applyYield ps yields
  where
    yields = concatMap tileYield (matchingTiles roll board)

    applyYield players (pid, res, amount) = map (giveResource pid res amount) players

    giveResource pid res amount (c, p)
        | playerId p == pid = (c, p { resources = addResource res amount (resources p) })
        | otherwise         = (c, p)

addResource :: Resource -> Int -> Map.Map Resource Int -> Map.Map Resource Int
addResource res amount = Map.insertWith (+) res amount
------------------------------------------------------------------------------------------------------------------------------

-- Get a node from the board by NodeId
lookupNode :: NodeId -> Board -> Maybe Node
lookupNode nid (Board tileMap) =
    foldr findNode Nothing (Map.elems tileMap)
  where
    findNode tile acc = case filter ((== nid) . nodeId) (nodes tile) of
        (n:_) -> Just n
        []    -> acc

-- Get an edge from the board by EdgeId
lookupEdge :: EdgeId -> Board -> Maybe Edge
lookupEdge eid (Board tileMap) =
    foldr findEdge Nothing (Map.elems tileMap)
  where
    findEdge tile acc = case filter ((== eid) . edgeId) (edges tile) of
        (e:_) -> Just e
        []    -> acc