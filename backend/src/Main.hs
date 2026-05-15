module Main where 

-- Libs
import System.IO (hFlush, stdout)
import qualified Data.Map as Map 
import Control.Monad
import System.Random
import Data.Maybe 
import Data.List (intercalate, sortOn)
import BuildPhase
import Data.UUID

-- Local
import Types 
import Coordinates
import Util

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
    let (color, _) = players gs !! currentTurn gs
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
initGameState :: IO GameState
initGameState = do
    let newPlayers                = map initPlayer someUUIDs    -- from Util
    return GameState
        { gameId      = 0
        , board       = catanBoard
        , players     = zip [Red, Blue, Orange, White] newPlayers
        , currentTurn = 0
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
placePair gs (color, nid, eid) =
    let pid        = playerId . snd . head $ filter ((== color) . fst) (players gs)
        newBoard   = placeRoad eid pid $ placeSettlement nid pid (board gs)
        newPlayers = addRoadForced color (fromJust $ lookupEdge eid (board gs)) 
                    $ addSettlementForced color (fromJust $ lookupNode nid (board gs)) (players gs)
    in  gs { board = newBoard, players = newPlayers }

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
    ownsBuilding pid1 (Settlement pid2) = pid1 == pid2
    ownsBuilding pid1 (City pid2)       = pid1 == pid2

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
            buildPhase gs color

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
distributeResources roll brd ps =
    foldl applyYield ps yields
  where
    yields = concatMap tileYield (matchingTiles roll brd)

    applyYield plrs (pid, res, amount) = map (giveResource pid res amount) plrs

    giveResource pid res amount (c, p)
        | playerId p == pid = (c, p { resources = addResource res amount (resources p) })
        | otherwise         = (c, p)

addResource :: Resource -> Int -> Map.Map Resource Int -> Map.Map Resource Int
addResource res amount = Map.insertWith (+) res amount
