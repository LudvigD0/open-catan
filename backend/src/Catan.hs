module Catan where 
import Types
import Util
import Data.UUID
import Coordinates
import qualified Data.Map as Map 
import System.Random
import Data.Maybe (mapMaybe, isJust)
import Data.List (nub)
import Data.Map (elems, fromList, keys)

{-- 

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

s--}


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


getAllNodes :: Board -> [Node]
getAllNodes = nub . concatMap nodes . elems . tiles

diceResult :: IO (Int, Int)
diceResult = do dice1 <- randomRIO (1, 6)
                dice2 <- randomRIO (1, 6)
                return (dice1, dice2)

-- this function is for a filter (GraphTile -> Bool) 
getResourcesFromTiles :: (Tile -> Bool) -> Node -> [Resource]
getResourcesFromTiles f (Node _ b _ ts) = case b of
    Just (Settlement _) -> r
    Just (City _      ) -> r ++ r
    Nothing             -> []
    where r = mapMaybe resource (filter f ts)

getResourcesOfNum :: Int -> Player -> [Resource]
getResourcesOfNum n p = concatMap (getResourcesFromTiles hasToken) $ buildingFilter (buildings p)
    where
        hasToken tile = n == token tile
        
        buildingFilter = filter hasBuilding -- It just has to be here since haskell has to 100% make sure the building exists
        hasBuilding (Node _ b _ _) = isJust b

-- Updaters
placeBuilding :: Board -> Building -> NodeId -> Board
placeBuilding b build idn = undefined

{-
TODO:
replaceNodes :: Board -> (Node -> Node) -> [NodeId] -> Board
replaceNodes b f idns = Board { tiles = fromList (zip (keys (tiles b)) newMap) }
    where
        newMap = map newTile b
        
        newTile t = if foldr isChangeNode False . nodes t
            then t { nodes = map newNode (nodes t) }
            else t

        newNode n = if isChangeNode n
            then f n
            else n
        
        isChangeNode (Node idn _ _ _) = idn `elem` idns

replaceEdges :: Board -> (Edge -> Edge) -> [EdgeId] -> Board
replaceEdges b ides = Board { tiles = newMap }
    where
        newMap = undefined
-}