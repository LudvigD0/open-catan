-- | Pure validation checks for Catan placement and resource rules.
module ValidationLogic
  ( validCityPlacement
  , validStlmPlacement
  , validRoadPlacement
  , hasConnectingRoad
  , checkSettlementRes
  , checkRoadRes
  , checkCityRes
  ) where

import Data.Maybe (isNothing)
import qualified Data.Map as Map 

import Types 
import PureUtil

-- * Placement validation

-- | A city can only be built on the current player's existing settlement.
validCityPlacement :: NodeId -> PlayerId -> Board -> Bool 
validCityPlacement nid pid brd =
    case building =<< lookupNode nid brd of
        Just (Settlement nodepid) -> pid == nodepid
        _                         -> False

-- | A settlement needs an empty node, empty adjacent nodes, and an own road.
validStlmPlacement :: NodeId -> PlayerId -> Board -> Bool
validStlmPlacement nid pid brd =
    maybe False canBuildSettlement (lookupNode nid brd)
  where
    canBuildSettlement node =
        isNothing (building node) &&
        all (isNothing . building) (adjacentNodes node brd) &&
        hasConnectingRoad node pid brd

-- | Check that an edge connects to the player's building or road network.
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

-- | Check whether the player owns a road touching the given node.
hasConnectingRoad :: Node -> PlayerId -> Board -> Bool
hasConnectingRoad node pid brd = any isOwnRoad (nodeEdges node)  
  where
    isOwnRoad eid = case road =<< lookupEdge eid brd of
        Just (Road ownerId) -> ownerId == pid
        Nothing             -> False

-- * Resource validation

-- | Check whether the player has the resources required for a settlement.
checkSettlementRes :: GameState -> Color -> Bool
checkSettlementRes gs color = all (>= 1) [nLumber, nGrain, nBrick, nWool]
  where
    res     = resources $ getPlayer color gs 
    nLumber = res Map.! Lumber
    nGrain  = res Map.! Grain
    nBrick  = res Map.! Brick
    nWool   = res Map.! Wool
    
-- | Check whether the player has the resources required for a road.
checkRoadRes :: GameState -> Color -> Bool
checkRoadRes gs color = all (>= 1) [nLumber, nBrick]
  where
    res     = resources $ getPlayer color gs
    nLumber = res Map.! Lumber
    nBrick  = res Map.! Brick

-- | Check whether the player has the resources required for a city upgrade.
checkCityRes :: GameState -> Color -> Bool
checkCityRes gs color = nOre >= 3 && nGrain >= 2
  where
    res     = resources $ getPlayer color gs
    nOre   = res Map.! Ore
    nGrain = res Map.! Grain
