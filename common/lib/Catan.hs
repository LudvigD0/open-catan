module Catan where 
import Types
--import Util ()
import Data.UUID.Types
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
    1. Initialize empty board                 -- Done 
    2. Initialize Player state                -- Done 
    3. Auto place first buildings and roads   -- Done
    4. Random player selected to start        -- Done 

- Turn Loop 
    5. 2 Dice role - two paths
    6. Resource distribution
    7. Building phase  (Road, Settelment, City)
    8. Check VP >= 10 -> Endgame 
    9. Next player, repeat from 5 

s--}


-- 1. Game state and Initializing board 

-- Main function for SINGLE game instance 
{- main :: IO ()
main = do
    gs <- initGameState
    let gs' = autoPlace gs
    putStrLn "Starting Catan!"
    printBoard (board gs')
    gameLoop gs' -}

--placePair :: GameState -> (Color, NodeId, EdgeId) -> GameState
--placePair state (color, nid, eid) =
--    let pid        = playerId . snd . head $ filter ((== color) . fst) (players state)
--        newBoard   = placeRoad eid pid $ placeSettlement nid pid (board state)
--        newPlayers = addRoad color eid $ addSettlement color nid (players state)
--    in  state { board = newBoard, players = newPlayers }

--Viktor, här har jag ändrat placePair lite för att kunna köra cabal.build
--Om du anser att det är fel är det bara att ta tillbaka det jag har 
--kommenterat ut.

-- 1. game loop and rules





------------------------------------------------------------------------------------------








{- 
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
 -}