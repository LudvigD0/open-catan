module GameActions
    ( applyGameAction
    , startGame
    ) where

-- Libs
import qualified Data.Map as Map
import Data.List (elemIndex)
import Data.Maybe (fromJust, isJust)
import System.Random (randomRIO)

-- Local
import BuildPhase
import Types
import Util
import Catan

-- New board state with starter placements 
startGame :: GameState
startGame = autoPlace (initGameState someUUIDs 0)

{- GameActions for the API 
   - Takes a GameAction and GameState as argument 
   - If GameAction is valid for phase, apply correct function to Gamestate
   - Otherwise returns an invalid GameAction  (GameError)
   - If corresponding function for GameAction is successfull, returns new gamestate
   - Otherwise returns Nothing, and returns an InvalidAction
 -}

applyGameAction :: GameAction -> GameState -> IO (Either GameError GameState)
applyGameAction ga gs =
    case ga of
        ActRollDice
            | turnPhase gs == Roll -> do
                diceRoll <- rollDice
                case applyRollDice diceRoll gs of
                    Nothing -> return $ Left InvalidGameAction
                    Just gs' -> return $ Right gs'
            | otherwise ->
                return $ Left (InvalidPhase (turnPhase gs) ga)
        ActBuildRoad eid
            | turnPhase gs == Build ->
                case buildRoad eid gs of
                    Nothing -> return $ Left InvalidRoadPlacement
                    Just gs' -> return $ Right gs'
            | otherwise ->
                return $ Left (InvalidPhase (turnPhase gs) ga)
        ActBuildSettlement nid
            | turnPhase gs == Build ->
                case buildSettlement nid gs of
                    Nothing -> return $ Left InvalidSettlementPlacement
                    Just gs' -> return $ Right gs'
            | otherwise ->
                return $ Left (InvalidPhase (turnPhase gs) ga)
        ActBuildCity nid
            | turnPhase gs == Build ->
                case buildCity nid gs of
                    Nothing -> return $ Left InvalidCityPlacement
                    Just gs' -> return $ Right gs'
            | otherwise ->
                return $ Left (InvalidPhase (turnPhase gs) ga)
        ActEndTurn
            | turnPhase gs == Build ->
                case endTurn gs of
                    Nothing -> return $ Left InvalidGameAction
                    Just gs' -> return $ Right gs'
            | otherwise ->
                return $ Left (InvalidPhase (turnPhase gs) ga)

-- rollDice is called from applyGameAction, sent in as argument
applyRollDice :: (Int, Int) -> GameState -> Maybe GameState
applyRollDice roll@(r1, r2) gs =
    let newPlayers = distributeResources (r1 + r2) (board gs) (players gs)
    in Just gs
        { dice = roll
        , players = newPlayers
        , turnPhase = Build
        }

-- Checks for valid edgeid, resources, then adds to board and player state. Otherwise returns Nothing 
buildRoad :: EdgeId -> GameState -> Maybe GameState
buildRoad eid gs =
    case lookupEdge eid (board gs) of
        Nothing -> Nothing
        Just edge
            | isJust (road edge) -> Nothing
            | not (checkRoadRes gs color) -> Nothing
            | not (validRoadPlacement edge pid (board gs)) -> Nothing
            | otherwise ->
                let newBoard = placeRoad eid pid (board gs)
                    newPlayers = addRoad color eid (players gs)
                in Just gs { board = newBoard, players = newPlayers }
  where
    color = currentTurn gs
    pid = playerId $ getPlayer color gs

-- Checks that an edge is connected to the player's existing road network.
-- A road may start from one of the player's buildings or extend from one of
-- their roads touching either endpoint of the chosen edge.
validRoadPlacement :: Edge -> PlayerId -> Board -> Bool
validRoadPlacement edge pid brd =
    any hasOwnBuilding endpointNodes || any hasOwnRoad adjacentEdgeIds
  where
    (n1, n2) = edgeNodes edge
    endpointNodes = map (`lookupNode` brd) [n1, n2]
    adjacentEdgeIds =
        concatMap (maybe [] nodeEdges) endpointNodes

    hasOwnBuilding (Just node) =
        case building node of
            Just (Settlement ownerId) -> ownerId == pid
            Just (City ownerId)       -> ownerId == pid
            Nothing                   -> False
    hasOwnBuilding Nothing = False

    hasOwnRoad eid =
        case road =<< lookupEdge eid brd of
            Just (Road ownerId) -> ownerId == pid
            Nothing             -> False

-- Checks for valid nodeid, resources, then adds to board and player state. Otherwise returns Nothing 
buildSettlement :: NodeId -> GameState -> Maybe GameState
buildSettlement nid gs
    | not (checkSettlementRes gs color) = Nothing
    | not (validStlmPlacement nid pid (board gs)) = Nothing
    | otherwise =
        let newBoard = placeSettlement nid pid (board gs)
            newPlayers = addSettlement color nid (players gs)
        in Just gs { board = newBoard, players = newPlayers }
  where
    color = currentTurn gs
    pid = playerId $ getPlayer color gs

-- Checks for valid nodeid, resources, then adds to board and player state. Otherwise returns Nothing 
buildCity :: NodeId -> GameState -> Maybe GameState
buildCity nid gs
    | not (checkCityRes gs color) = Nothing
    | not (validCityPlacement nid pid (board gs)) = Nothing
    | otherwise =
        let newBoard = placeCity nid pid (board gs)
            newPlayers = addCity color (players gs)
        in Just gs { board = newBoard, players = newPlayers }
  where
    color = currentTurn gs
    pid = playerId $ getPlayer color gs

-- Updates currentTurn, resets dice 
endTurn :: GameState -> Maybe GameState
endTurn gs =
    Just gs
        { currentTurn = nextPlayer (currentTurn gs) (players gs)
        , turnPhase = Roll
        , dice = (0, 0)
        }

nextPlayer :: Color -> Map.Map Color Player -> Color
nextPlayer current ps =
    let keys        = Map.keys ps
        currentIdx  = fromJust $ elemIndex current keys
        nextIdx     = (currentIdx + 1) `mod` length keys
    in  keys !! nextIdx

rollDice :: IO (Int, Int)
rollDice = do
    d1 <- randomRIO (1, 6)
    d2 <- randomRIO (1, 6)
    return (d1, d2)

matchingTiles :: Int -> Board -> [Tile]
matchingTiles roll (Board tileMap _ _) =
    [ tile | tile <- Map.elems tileMap,
      token tile == roll,
      not (robber tile),
      resource tile /= Nothing]

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

-- Gathers tiles with matching token (Int), creates triple (pid, resource, amount) from nodes with building 
-- then updates players resources 
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
