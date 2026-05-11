module Catan where 
import Types
import Util
import Data.UUID
import Coordinates
import qualified Data.Map as Map 
import System.Random
import Data.Maybe (mapMaybe, isJust, Maybe (Nothing))
import Data.List (nub)
import Data.Map (elems, fromList, keys)
import Data.Foldable (concat)
import GHC.Base (undefined, build)

{-- 

-- MVP 

- Init Game state
    1. Initialize empty board                 -- Done 
    2. Initialize Player state                -- Done 
    3. Auto place first buildings and roads   -- Done
    4. Random player selected to start        -- 

- Turn Loop 
    5. 2 Dice role - two paths
    6. Resource distribution
    7. Building phase  (Road, Settelment, City)
    8. Check VP >= 10 -> Endgame 
    9. Next player, repeat from 5 

s--}


-- 1. Game state and Initializing board 

-- Main function for SINGLE game instance 
main :: IO GameState
main = do
    gs <- initGameState
    let gs' = autoPlace gs 
    return gs'

    

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
        , specialKnight = (head newPlayers, 0)
        , specialRoad = (head newPlayers, 0)
        }

initPlayer :: UUID -> Player
initPlayer uid = Player
    { playerId  = PlayerId uid
    , points    = 0
    , buildings = []
    , roads   = []
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

autoPlace :: GameState -> GameState
autoPlace gs = foldl placePair gs allPlacements
  where
    allPlacements =
        [ (color, nid, eid)
        | (color, pairs) <- beginnerPlacements, (nid, eid) <- pairs]

--placePair :: GameState -> (Color, NodeId, EdgeId) -> GameState
--placePair state (color, nid, eid) =
--    let pid        = playerId . snd . head $ filter ((== color) . fst) (players state)
--        newBoard   = placeRoad eid pid $ placeSettlement nid pid (board state)
--        newPlayers = addRoad color eid $ addSettlement color nid (players state)
--    in  state { board = newBoard, players = newPlayers }

--Viktor, här har jag ändrat placePair lite för att kunna köra cabal.build
--Om du anser att det är fel är det bara att ta tillbaka det jag har 
--kommenterat ut.

placePair :: GameState -> (Color, NodeId, EdgeId) -> GameState
placePair state (color, nid, eid) =
    let pid      = playerId . snd . head $ filter ((== color) . fst) (players state)
        newBoard = placeRoad eid pid $ placeSettlement nid pid (board state)
    in  state { board = newBoard }

updatePlayer :: Color -> Node -> Edge -> [(Color, Player)] -> [(Color, Player)] --Innan stod det NodeId som andra parameter och som skickades till buildings
updatePlayer color node edge = map update
  where
    update (c, p)
        | c == color = (c, p { buildings = node : buildings p
                              , roads     = edge : roads p
                              })
        | otherwise  = (c, p)

addSettlement :: Color -> Node -> [(Color, Player)] -> [(Color, Player)]  --Innan stod det NodeId som andra parameter men nu Node
addSettlement color node = map update
  where
    update (c, p)
        | c == color = (c, p { buildings = node : buildings p })
        | otherwise  = (c, p)

addRoad :: Color -> Edge -> [(Color, Player)] -> [(Color, Player)]  --Innan stod det EdgeId som andra parameter men nu Edge
addRoad color edge = map update
  where
    update (c, p)
        | c == color = (c, p { roads = edge : roads p })
        | otherwise  = (c, p)

placeSettlement :: NodeId -> PlayerId -> Board -> Board
placeSettlement nid pid (Board tileMap) = Board $ Map.map updateTile tileMap
  where
    updateTile tile = tile { nodes = map updateNode (nodes tile) }
    updateNode n
        | nodeId n == nid = n { building = Just (Settlement pid) }
        | otherwise       = n

placeCity :: NodeId -> PlayerId -> Board -> Board
placeCity nid pid (Board tileMap) = Board $ Map.map updateTile tileMap
  where
    updateTile tile = tile { nodes = map updateNode (nodes tile) }
    updateNode n
        | nodeId n == nid = n { building = Just (City pid) }
        | otherwise       = n

placeRoad :: EdgeId -> PlayerId -> Board -> Board
placeRoad eid pid (Board tileMap) = Board $ Map.map updateTile tileMap
  where
    updateTile tile = tile { edges = map updateEdge (edges tile) }
    updateEdge e
        | edgeId e == eid = e { road = Just (Road pid) }
        | otherwise       = e

nextTurn :: TurnPhase -> TurnPhase 
nextTurn Roll = Build 
nextTurn Build = Trade
nextTurn Trade = Roll


-- 1. game loop and rules

gameLoop :: GameState -> IO ()
gameLoop gs = do  --lagt till gs här (GameState) och retunerar det nu där nere för att inte få error
    (d1, d2) <- diceResult
    --givePlayersResources (d1 + d2)        TEMPORÄRT KOMMENTERAD BORT FÖR ATT KOMPILERA SPELET
    -- Place buildings with some updaters using Player input
    -- check VP
    -- update GameState
    gameLoop gs

diceResult :: IO (Int, Int)
diceResult = do dice1 <- randomRIO (1, 6)
                dice2 <- randomRIO (1, 6)
                return (dice1, dice2)

calcVictoryPoints :: Player -> GameState -> Int
calcVictoryPoints p gs = calcBuildingPoints p 
                       + calcVictoryCards p
                       + calcSpecialCards p gs

calcBuildingPoints :: Player -> Int
calcBuildingPoints = sum . map buildingPoints . buildings

calcVictoryCards :: Player -> Int
calcVictoryCards = victoryCards

calcSpecialCards :: Player -> GameState -> Int
calcSpecialCards gs p = (if checkLargestArmy gs p then 2 else 0) 
                      + (if checkLongestRoad gs p then 2 else 0) 

buildingPoints :: Building -> Int
buildingPoints (Settlement _) = 1
buildingPoints (City _) = 2

checkLargestArmy :: GameState -> Player -> Bool
checkLargestArmy gs = fst specialKnight gs == playerId

checkLongestRoad :: GameState -> Player -> Bool
checkLongestRoad gs = fst specialRoad gs == playerId




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