module Util where 

-- Libs
import qualified Data.Map as Map 
import Data.UUID.Types
import System.Random

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
lookupNode nid (Board tileMap) =
    foldr findNode Nothing (Map.elems tileMap)
  where
    findNode tile acc = case filter ((== nid) . nodeId) (nodes tile) of
        (n:_) -> Just n
        []    -> acc

-- Get an edge from the board by EdgeId
lookupEdge :: EdgeId -> Board -> Maybe Edge
lookupEdge eid (Board tileMap) =
    foldr findEdge Nothing (Map.elems tileMap)
  where
    findEdge tile acc = case filter ((== eid) . edgeId) (edges tile) of
        (e:_) -> Just e
        []    -> acc

-- Get a player by color
getPlayer :: Color -> GameState -> Player
getPlayer color gs = snd . head $ filter ((== color) . fst) (players gs)

-- 
adjacentNodes :: Node -> Board -> [Node]
adjacentNodes node b =
    [ n | e <- nodeEdges node, let (n1, n2) = edgeNodes e
    , nid <- [nodeId n1, nodeId n2]
    , nid /= nodeId node
    , Just n <- [lookupNode nid b]
    ]