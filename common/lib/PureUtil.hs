-- | Pure lookup and graph helper functions shared by the game rules.
module PureUtil
  ( someUUIDs
  , oneUUID
  , lookupNode
  , lookupEdge
  , lookupTile
  , getPlayer
  , adjacentNodes
  ) where

import Data.Maybe (maybeToList)
import qualified Data.Map as Map 
import Data.UUID (UUID)
import System.Random (mkStdGen, random)

import Types

-- | Deterministic UUIDs used for local demos and tests.
someUUIDs :: [UUID]
someUUIDs =
  let seed = 123
      g0 = mkStdGen seed -- RNG from seed
      (u1, g1) = random g0
      (u2, g2) = random g1
      (u3, g3) = random g2
      (u4, _) = random g3
  in [u1,u2,u3,u4]

-- | Deterministic UUID used by tests and examples that need one player id.
oneUUID :: UUID
oneUUID = fst $ random g0 
 where 
    g0 = mkStdGen 125

-- | Look up a board node by id.
lookupNode :: NodeId -> Board -> Maybe Node
lookupNode nid brd = Map.lookup nid (nodes brd)

-- | Look up a board edge by id.
lookupEdge :: EdgeId -> Board -> Maybe Edge
lookupEdge eid brd = Map.lookup eid (edges brd)

-- | Look up a tile by cube coordinate.
lookupTile :: Cord -> Board -> Maybe Tile 
lookupTile crd brd = Map.lookup crd (tiles brd)


-- | Get a player by color.
getPlayer :: Color -> GameState -> Player
getPlayer color gs = players gs Map.! color 

-- | All board nodes directly connected to the given node by one edge.
adjacentNodes :: Node -> Board -> [Node]
adjacentNodes node brd =
    concatMap edgeNeighbors (nodeEdges node)
  where
    edgeNeighbors eid =
        case Map.lookup eid (edges brd) of
            Nothing -> []
            Just edge ->
                let (n1, n2) = edgeNodes edge
                    other
                        | n1 == nodeId node = Just n2
                        | n2 == nodeId node = Just n1
                        | otherwise         = Nothing
                in maybe [] (\nid -> maybeToList $ lookupNode nid brd) other
