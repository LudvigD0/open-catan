module Catan where 
import Types
import Util
import Data.UUID
import Coordinates
import qualified Data.Map as Map 

{-- 
- Init Game state
    1. Initialize empty board 
    2. Initialize Player state (Color, 5 settelments, 4 cities, 15 roads) 
    3. (Later) Players places 1 settelment and 1 road, repeat 2 for each player 

    4. Random player selected to start 
- Turn Loop 
    5. 2 Dice role - two paths
    6a. Resource distribution    |    6b. (Later) Robber placement and steal random card 
    7. (Later) Trading phase
    8. Building phase  (Road, Settelment, City, (Later) Dev card )
    9.  (Later) Play dev card 
    10. (Later) Update special cards (Longest road, Largest Army)
    11. Check VP > 10 -> Endgame 
    12. Next turn, repeat from 5 


-- MVP 

- Init Game state
    1. Initialize empty board 
    2. Initialize Player state (Color, 5 settelments, 4 cities, 15 roads) 
    3. Auto place first buildings and roads
    4. Random player selected to start 

- Turn Loop 
    5. 2 Dice role - two paths
    6. Resource distribution
    7. Building phase  (Road, Settelment, City)
    8. Check VP >= 10 -> Endgame 
    9. Next player, repeat from 5 

    --}


-- 1. Game state and Initializing board 

-- Main function for SINGLE game instance 
main :: IO ()
main = do
    (GameState _ b ps _ _ _) <- initGameState
    print [pid | (_, (Player pid _ _ _ )) <- ps ]  -- test player ids 
    print b

initGameState :: IO GameState
initGameState = do
    let newPlayers                = map initPlayer someUUIDs    -- from Util
    return GameState
        { gameId      = 0
        , board       = initBoard
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
    }

initBoard :: Board
initBoard = Board { tiles = Map.fromList catanTiles }

nextTurn :: TurnPhase -> TurnPhase 
nextTurn Roll = Build 
nextTurn Build = Trade
nextTurn Trade = Roll   