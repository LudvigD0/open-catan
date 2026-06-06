-- | Pure Catan game rules and state transitions.
module Catan
  ( initGameState
  , initPlayer
  , autoPlace
  , beginnerPlacements
  , nextPlayer
  , endTurn
  , buildRoad
  , buildSettlement
  , buildCity
  , countVP
  , checkWinner
  , tileYield
  , distributeResources
  , addResource
  , matchingTiles
  ) where

import Data.List (elemIndex)
import qualified Data.Map as Map 
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.UUID.Types (UUID)

import Coordinates (catanBoard)
import Types 
import StateLogic
import PureUtil
import ValidationLogic

-- | Create a game state with the given player UUIDs and starting player index.
initGameState :: [UUID] -> Int -> GameState
initGameState ids start =
    let newPlayers                = map initPlayer ids 
        colors = [Red, Blue, Orange, White]
    in GameState
        { gameId      = 0
        , board       = catanBoard
        , players     = Map.fromList $ zip colors newPlayers
        , currentTurn = colors !! start 
        , turnPhase   = Roll
        , dice        = (0, 0)
        }

-- | Create an empty player record for a UUID.
initPlayer :: UUID -> Player
initPlayer uid = Player
    { playerId  = PlayerId uid
    , points    = 0
    , buildings = []
    , roads   = []
    , resources = Map.fromList [(res, 0) | res <- [Lumber, Ore, Grain, Brick, Wool]]
    }

-- | Add the fixed initial settlements and roads for all players.
autoPlace :: GameState -> GameState
autoPlace gs = foldl placePair gs allPlacements
  where
    allPlacements =
        [ (color, nid, eid)
        | (color, pairs) <- beginnerPlacements, (nid, eid) <- pairs]


-- | Add one initial settlement-road pair without resource costs.
placePair :: GameState -> (Color, NodeId, EdgeId) -> GameState
placePair gs (color, nid, eid) =
    let pid        = playerId $ players gs Map.! color
        newBoard   = placeRoad eid pid $ placeSettlement nid pid (board gs)
        newPlayers = addRoadForced color eid $ addSettlementForced color nid (players gs)
    in  gs { board = newBoard, players = newPlayers }


-- | Fixed beginner placements, grouped by player color.
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

-- | Return the next player color in the map's turn order.
nextPlayer :: Color -> Map.Map Color Player -> Color
nextPlayer current ps =
    let keys        = Map.keys ps  
        currentIdx  = fromJust $ elemIndex current keys
        nextIdx     = (currentIdx + 1) `mod` length keys
    in  keys !! nextIdx
  
-- | Advance to the next player and reset turn-local dice state.
endTurn :: GameState -> Maybe GameState
endTurn gs =
    Just gs
        { currentTurn = nextPlayer (currentTurn gs) (players gs)
        , turnPhase = Roll
        , dice = (0, 0)
        }

-- * Build actions

-- | Build a road for the current player when placement and resources allow it.
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


-- | Build a settlement for the current player when placement and resources allow it.
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

-- | Upgrade a settlement to a city for the current player when resources allow it.
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

-- * Victory points

-- | Count victory points from the player's settlements and cities.
countVP :: Player -> Board -> Int
countVP p brd =
    sum $ map vpFor buildingsOwned
  where
    buildingsOwned =
        mapMaybe (`Map.lookup` nodes brd) (buildings p)

    vpFor node =
        case building node of
            Just (Settlement _) -> 1
            Just (City _)       -> 2
            _                   -> 0

-- | Return the first player with at least 10 victory points, if any.
checkWinner :: GameState -> Maybe Color
checkWinner gs =
    foldl check Nothing (Map.toList $ players gs)
  where
    check (Just winner) _      = Just winner
    check Nothing (color, p)
        | countVP p (board gs) >= 10 = Just color
        | otherwise                   = Nothing

-- * Resource production

-- | Resources produced by one tile for each adjacent building owner.
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

-- | Distribute resources from all tiles matching the dice roll.
distributeResources :: Int -> Board -> Map.Map Color Player -> Map.Map Color Player
distributeResources roll brd ps = Map.fromList $
    foldl applyYield (Map.toList ps) yields
  where
    yields = concatMap (`tileYield` brd) (matchingTiles roll brd)

    applyYield plrs (pid, res, amount) = map (giveResource pid res amount) plrs

    giveResource pid res amount (c, p)
        | playerId p == pid = (c, p { resources = addResource res amount (resources p) })
        | otherwise         = (c, p)

-- | Add an amount of one resource to a resource map.
addResource :: Resource -> Int -> Map.Map Resource Int -> Map.Map Resource Int
addResource res amount = Map.insertWith (+) res amount

-- | Find all non-robber resource tiles that match the dice roll token.
matchingTiles :: Int -> Board -> [Tile]
matchingTiles roll (Board tileMap _ _) =
    [ tile | tile <- Map.elems tileMap,
      token tile == roll,
      not (robber tile),
      isJust (resource tile)]
