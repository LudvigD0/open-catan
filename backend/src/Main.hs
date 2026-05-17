module Main where 

-- Libs
import System.IO (hFlush, stdout)
import qualified Data.Map as Map 
import Control.Monad
import System.Random
import Data.Maybe 
import Data.List (elemIndex)
import BuildPhase

-- Local
import Types 
import Util
import Catan (initGameState, autoPlace, getPlayer)

main :: IO ()
main = do
    start <- randomRIO (0, 3)
    let gs = initGameState someUUIDs start
    let gs' = autoPlace gs
    putStrLn "Starting Catan!"
    gameLoop gs'

-- 1. game loop and rules
gameLoop :: GameState -> IO ()
gameLoop gs = do
    printBoard (board gs)
    let color = currentTurn gs
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
            let gs4 = gs3 { currentTurn = nextPlayer (currentTurn gs3) (players gs3) }
            gameLoop gs4

nextPlayer :: Color -> Map.Map Color Player -> Color
nextPlayer current ps =
    let keys        = Map.keys ps  
        currentIdx  = fromJust $ elemIndex current keys
        nextIdx     = (currentIdx + 1) `mod` length keys
    in  keys !! nextIdx
    
------------------------------------------------ Initaialization ------------------------------------------------------------------------------


-----
--Init functions are now moved to common/lib

---





------------------------------------------------VP Check------------------------------------------------------------------------------

-- Goes through every node in each tile and checks if building is 
-- present, then checks if it belongs to the given player. 
-- Calculates sum of points from only settlements and cities
-- TODO: Add Longest Road and largest army 
countVP :: Player -> Board -> Int
countVP p board =
    sum $ map vpFor buildingsOwned
  where
    buildingsOwned =
        mapMaybe (`Map.lookup` nodes board) (buildings p)

    vpFor node =
        case building node of
            Just (Settlement _) -> 1
            Just (City _)       -> 2
            _                   -> 0

-- Goes through the current GameState, if a winner is found the other players are ignored 
-- Returns the color of the winning player 
checkWinner :: GameState -> Maybe Color
checkWinner gs =
    foldl check Nothing (Map.toList $ players gs)
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
printBoard (Board tileMap _ _) = do
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
    let player = getPlayer gs color
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
        "1" | checkSettlementRes gs color -> return gs -- placeSettlementIO gs color
        "2" | checkRoadRes gs color       -> return gs --placeRoadIO gs color
        "3" | checkCityRes gs color       -> return gs --placeCityIO gs color
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
matchingTiles roll (Board tileMap _ _) =
    [ tile | tile <- Map.elems tileMap,
      token tile == roll,
      not (robber tile),
      resource tile /= Nothing]

-- Given a tile, find which players have settlements/cities on it
-- and what resource they should receive
tileYield :: Tile -> Board -> [(PlayerId, Resource, Int)]
tileYield tile brd =
    case resource tile of
        Nothing  -> []
        Just res -> concatMap yieldFromNode (tileNodes tile)
          where
            yieldFromNode nid =
                case Map.lookup nid (nodes brd) of
                    Nothing   -> []
                    Just node ->
                        case building node of
                            Just (Settlement pid) -> [(pid, res, 1)]
                            Just (City pid)       -> [(pid, res, 2)]
                            Nothing               -> []

-- Distribute resources to all players based on dice roll
distributeResources :: Int -> Board -> Map.Map Color Player -> Map.Map Color Player
distributeResources roll brd ps = Map.fromList $ 
    foldl applyYield (Map.toList ps) yields
  where
    yields = concatMap (`tileYield` brd) (matchingTiles roll brd)

    applyYield plrs (pid, res, amount) = map (giveResource pid res amount) plrs

    giveResource pid res amount (c, p)
        | playerId p == pid = (c, p { resources = addResource res amount (resources p) })
        | otherwise         = (c, p)

addResource :: Resource -> Int -> Map.Map Resource Int -> Map.Map Resource Int
addResource res amount = Map.insertWith (+) res amount
