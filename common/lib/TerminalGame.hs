-- | Minimal terminal demo for exercising the pure game rules locally.
module TerminalGame
  ( main
  , gameLoop
  , printResources
  , printBoard
  , buildPhase
  , rollDice
  ) where

import qualified Data.Map as Map 
import System.IO (hFlush, stdout)
import System.Random (randomRIO)

import Types 
import Catan
import PureUtil
import ValidationLogic

-- | Run the terminal demo with deterministic players and random starting color.
main :: IO ()
main = do
    start <- randomRIO (0, 3)
    let gs = initGameState someUUIDs start
    let gs' = autoPlace gs
    putStrLn "Starting Catan!"
    gameLoop gs'

-- | Repeatedly run turns until a player reaches the victory point target.
gameLoop :: GameState -> IO ()
gameLoop gs = do
    printBoard (board gs)
    let color = currentTurn gs
    putStrLn $ "\n====== " ++ show color ++ "'s turn ======"

    -- Roll dice
    (d1, d2) <- rollDice
    let roll = d1 + d2
    putStrLn $ "Rolled: " ++ show roll
    -- Update game state.
    let gs1 = gs { dice = (d1, d2) }

    -- Distribute resources
    let newPlayers = distributeResources roll (board gs1) (players gs1)
        gs2 = gs1 { players = newPlayers }
    putStrLn "Resources distributed."

    -- Build phase
    gs3 <- buildPhase gs2 color

    -- Step 8: Check VP
    case checkWinner gs3 of
        Just winner -> putStrLn $ "\n" ++ show winner ++ " wins!"
        Nothing     -> do
            let gs4 = gs3 { currentTurn = nextPlayer (currentTurn gs3) (players gs3) }
            gameLoop gs4

-- | Print a player's current resource inventory.
printResources :: Player -> IO ()
printResources p = do
    putStrLn "\nYour resources:"
    mapM_ printRes (Map.toList (resources p))
  where
    printRes (res, n) = putStrLn $ "  " ++ show res ++ ": " ++ show n

-- | Print a compact tile-by-tile view of the board.
printBoard :: Board -> IO ()
printBoard (Board tileMap _ _) = do
    putStrLn "\n--- Board State ---"
    mapM_ printTile (Map.elems tileMap)
  where
    printTile t = putStrLn $
        "Tile " ++ show (tileId t) ++
        " [" ++ maybe "Desert" show (resource t) ++ "]" ++
        " token=" ++ show (token t)

-- | Prompt for one build-phase action.
--
-- The terminal demo currently skips interactive placement and leaves full build
-- handling to the API-oriented action flow.
buildPhase :: GameState -> Color -> IO GameState
buildPhase gs color = do
    let player = getPlayer color gs 
    printResources player
    putStrLn "\nBuild phase - choose an action:"
    putStrLn "  4. Skip"
    putStr "Choice: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" | checkSettlementRes gs color -> return gs --placeSettlementIO gs color
        "2" | checkRoadRes gs color       -> return gs --placeRoadIO gs color
        "3" | checkCityRes gs color       -> return gs --placeCityIO gs color
        "4"                               -> return gs
        _   -> do
            putStrLn "Invalid choice or insufficient resources."
            buildPhase gs color

-- | Roll two six-sided dice.
rollDice :: IO (Int, Int)
rollDice = do
    d1 <- randomRIO (1, 6)
    d2 <- randomRIO (1, 6)
    return (d1, d2)
