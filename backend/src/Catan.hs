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
import Data.Foldable (concat)
import GHC.Base (undefined)

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
    print [pid | (_, (Player pid _ _ _ _)) <- ps ]  -- test player ids 
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


-- 1. game loop and rules

gameLoop :: GameState -> IO ()
gameLoop = do
    (d1, d2) <- diceResult
    givePlayersResources (d1 + d2)
    -- Place buildings with some updaters using Player input
    -- check VP
    -- update GameState
    gameLoop undefined


diceResult :: IO (Int, Int)
diceResult = do dice1 <- randomRIO (1, 6)
                dice2 <- randomRIO (1, 6)
                return (dice1, dice2)

-- the function input is a filter, use f = True if you don't need it.
getResourcesFromTiles :: (Tile -> Bool) -> Node -> [Resource]
getResourcesFromTiles f n = case building n of
    Just (Settlement _) -> r
    Just (City _      ) -> r ++ r
    Nothing             -> []
    where r = mapMaybe resource $ filter f $ nodeTiles n

getResourcesOfNum :: Int -> Player -> [Resource]
getResourcesOfNum n p = concatMap (getResourcesFromTiles hasToken) $ buildingFilter (buildings p)
    where
        hasToken tile = n == token tile && not (robber tile)
        buildingFilter = filter hasBuilding -- It just has to be here since haskell has to 100% make sure the building exists
        hasBuilding (Node _ b _ _) = isJust b

-- Updaters
givePlayersResources :: Int -> GameState -> GameState
givePlayersResources n gs = gs { players = newPlayers }
    where
        newPlayers p = map (addCards . snd)
        addCards = resourceCards p ++ getResourcesOfNum n

removePlayerResources :: Player -> ([Resource] -> [Resource]) -> GameState -> GameState
removePlayerResources p f = undefined -- TODO

placeRobber :: TileId -> Board -> Board
placeRobber idn = undefined -- TODO

placeBuilding :: Building -> NodeId -> Board -> Board
placeBuilding bld idn = replaceNodes (\x -> x { building = Just bld }) [idn]

placeRoad :: Road -> EdgeId -> Board -> Board
placeRoad ro ide = replaceEdges (\x -> x { road = Just ro }) [ide]

replaceTiles :: (Tile -> Tile) -> [TileId] -> Board -> Board
replaceTiles = Board { tiles = fromList (zip cords newMap) }
    where
        cords = keys (tiles b)
        newMap = map f b

replaceNodes :: (Node -> Node) -> [NodeId] -> Board -> Board
replaceNodes f idns b = Board { tiles = fromList (zip cords newMap) }
    where
        cords = keys (tiles b)
        newMap = map applyf b
        applyf t = map f . filter isChangeNode . nodes
        isChangeNode n = nodeId n `elem` idns

replaceEdges :: (Edge -> Edge) -> [EdgeId] -> Board -> Board
replaceEdges ides b = Board { tiles = fromList (zip cords newMap) }
    where
        cords = keys (tiles b)
        newMap = map applyf b
        applyf t = map f . filter isChangeEdge . edges . nodes
        isChangeEdge e = edgeId e `elem` ides