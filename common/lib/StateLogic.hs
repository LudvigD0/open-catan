-- | Pure state updates for the board and player records.
module StateLogic
  ( placeSettlement
  , placeCity
  , placeRoad
  , addCity
  , addSettlement
  , addRoad
  , addSettlementForced
  , addRoadForced
  ) where

import qualified Data.Map as Map 

import Types 

-- * Board updates

-- | Place a settlement owned by the given player on a board node.
placeSettlement :: NodeId -> PlayerId -> Board -> Board
placeSettlement nid pid brd =
    brd { nodes = Map.adjust updateNode nid (nodes brd) }
  where
    updateNode n = n { building = Just (Settlement pid) }

-- | Upgrade or place a city owned by the given player on a board node.
placeCity :: NodeId -> PlayerId -> Board -> Board
placeCity nid pid brd =
    brd { nodes = Map.adjust updateNode nid (nodes brd) }
  where
    updateNode n = n { building = Just (City pid) }

-- | Place a road owned by the given player on a board edge.
placeRoad :: EdgeId -> PlayerId -> Board -> Board
placeRoad eid pid brd =
    brd { edges = Map.adjust updateEdge eid (edges brd) }
  where
    updateEdge e = e { road = Just (Road pid) }


-- * Player updates

-- | Deduct the resources needed to upgrade one settlement to a city.
--
-- The building list is unchanged because a city replaces an existing settlement
-- on the same node.
addCity :: Color -> Map.Map Color Player -> Map.Map Color Player
addCity color = Map.adjust updatePlayer color
  where
    updatePlayer p =
        p { resources = updateResources (resources p) }

    updateResources =
        Map.mapWithKey deduct

    deduct res amount
        | res == Grain = amount - 2
        | res == Ore   = amount - 3
        | otherwise    = amount

-- | Add a settlement node to the player and deduct settlement resources.
addSettlement :: Color -> NodeId -> Map.Map Color Player -> Map.Map Color Player
addSettlement color nid plyrs = 
    Map.adjust updatePlayer color plyrs
  where
    updatePlayer p =
        p
          { buildings = nid : buildings p
          , resources = updateRes (resources p) 
          }

    updateRes resM = Map.mapWithKey deduct  resM
    deduct res i
        | res `elem` [Lumber, Grain, Brick, Wool] = i - 1
        | otherwise                               = i

-- | Add a road edge to the player and deduct road resources.
addRoad :: Color -> EdgeId -> Map.Map Color Player -> Map.Map Color Player
addRoad color eid plyrs =
    Map.adjust updatePlayer color plyrs
  where
    updatePlayer p =
        p
          { roads = eid : roads p
          , resources = updateRes (resources p)
          }

    updateRes resM = Map.mapWithKey deduct resM
    deduct res i
        | res `elem` [Lumber, Brick] = i - 1
        | otherwise                               = i
    
-- | Add a settlement without deducting resources, used for initial placement.
addSettlementForced :: Color -> NodeId -> Map.Map Color Player -> Map.Map Color Player
addSettlementForced color nid plyrs =
    Map.adjust updatePlayer color plyrs
  where
    updatePlayer p = p { buildings = nid : buildings p }

-- | Add a road without deducting resources, used for initial placement.
addRoadForced :: Color -> EdgeId -> Map.Map Color Player -> Map.Map Color Player
addRoadForced color eid plyrs =
    Map.adjust updatePlayer color plyrs
  where
    updatePlayer p = p { roads = eid : roads p }  
