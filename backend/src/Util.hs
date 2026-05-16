module Util where 

-- Libs
import qualified Data.Map as Map 
import Data.UUID.Types
import System.Random
import Data.Maybe

-- Local
import Types

someUUIDs :: [UUID]
someUUIDs =
  let seed = 123
      g0 = mkStdGen seed -- RNG from seed
      (u1, g1) = random g0
      (u2, g2) = random g1
      (u3, g3) = random g2
      (u4, _) = random g3
  in [u1,u2,u3,u4]

oneUUID :: UUID
oneUUID = fst $ random g0 
 where 
    g0 = mkStdGen 125

-- Get a node from the board by NodeId
lookupNode :: NodeId -> Board -> Maybe Node
lookupNode nid brd = Map.lookup nid (nodes brd)

-- Get an edge from the board by EdgeId
lookupEdge :: EdgeId -> Board -> Maybe Edge
lookupEdge eid brd = Map.lookup eid (edges brd)

lookupTile :: Cord -> Board -> Maybe Tile 
lookupTile crd brd = Map.lookup crd (tiles brd)

-- Get a player by color
getPlayer :: Color -> GameState -> Player
getPlayer color gs = (players gs) Map.! color 

-- 
adjacentNodes :: Node -> Board -> [Node]
adjacentNodes node brd =
    concatMap edgeNeighbors (nodeEdges node)
  where
    edgeNeighbors eid =
        case Map.lookup eid (edges brd) of
            Nothing -> []
            Just edge ->
                let (n1, n2) = edgeNodes edge
                    other =
                        if n1 == nodeId node then n2 else n1
                in maybeToList $ Map.lookup other (nodes brd)