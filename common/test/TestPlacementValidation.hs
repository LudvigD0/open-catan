module TestPlacementValidation (tests) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.UUID.Types (fromString)

import ValidationLogic
import PureUtil
import Types
    ( Board (..)
    , Building (..)
    , Edge (..)
    , EdgeId (..)
    , Node (..)
    , NodeId (..)
    , PlayerId (..)
    , Road (..)
    )

tests :: [(String, Bool)]
tests =
    [ ( "settlement placement allows empty node with connected own road"
      , validStlmPlacement (NodeId 1) playerOne validSettlementBoard
      )
    , ( "settlement placement rejects node without connected own road"
      , not $ validStlmPlacement (NodeId 1) playerOne noRoadBoard
      )
    , ( "settlement placement rejects occupied target node"
      , not $ validStlmPlacement (NodeId 1) playerOne occupiedTargetBoard
      )
    , ( "settlement placement rejects occupied adjacent node"
      , not $ validStlmPlacement (NodeId 1) playerOne occupiedAdjacentBoard
      )
    , ( "settlement placement rejects invalid node"
      , not $ validStlmPlacement (NodeId 99) playerOne validSettlementBoard
      )
    , ( "city placement allows upgrading own settlement"
      , validCityPlacement (NodeId 1) playerOne ownSettlementBoard
      )
    , ( "city placement rejects another player's settlement"
      , not $ validCityPlacement (NodeId 1) playerOne otherSettlementBoard
      )
    , ( "adjacentNodes returns nodes connected by listed edges"
      , map nodeId (adjacentNodes nodeOne validSettlementBoard) == [NodeId 2]
      )
    , ( "adjacentNodes ignores malformed edge references"
      , null $ adjacentNodes malformedNode malformedBoard
      )
    ]

playerOne :: PlayerId
playerOne = PlayerId $ fromJust $ fromString "00000000-0000-0000-0000-000000000001"

playerTwo :: PlayerId
playerTwo = PlayerId $ fromJust $ fromString "00000000-0000-0000-0000-000000000002"

validSettlementBoard :: Board
validSettlementBoard = boardWith
    [ nodeOne, nodeTwo, nodeThree ]
    [ roadEdge (EdgeId 1) playerOne (NodeId 1, NodeId 2)
    , emptyEdge (EdgeId 2) (NodeId 2, NodeId 3)
    ]

noRoadBoard :: Board
noRoadBoard = boardWith
    [ nodeOne, nodeTwo, nodeThree ]
    [ emptyEdge (EdgeId 1) (NodeId 1, NodeId 2)
    , emptyEdge (EdgeId 2) (NodeId 2, NodeId 3)
    ]

occupiedTargetBoard :: Board
occupiedTargetBoard = boardWith
    [ nodeOne { building = Just (Settlement playerOne) }, nodeTwo, nodeThree ]
    [ roadEdge (EdgeId 1) playerOne (NodeId 1, NodeId 2)
    , emptyEdge (EdgeId 2) (NodeId 2, NodeId 3)
    ]

occupiedAdjacentBoard :: Board
occupiedAdjacentBoard = boardWith
    [ nodeOne, nodeTwo { building = Just (Settlement playerTwo) }, nodeThree ]
    [ roadEdge (EdgeId 1) playerOne (NodeId 1, NodeId 2)
    , emptyEdge (EdgeId 2) (NodeId 2, NodeId 3)
    ]

ownSettlementBoard :: Board
ownSettlementBoard = boardWith
    [ nodeOne { building = Just (Settlement playerOne) }, nodeTwo, nodeThree ]
    [ emptyEdge (EdgeId 1) (NodeId 1, NodeId 2)
    , emptyEdge (EdgeId 2) (NodeId 2, NodeId 3)
    ]

otherSettlementBoard :: Board
otherSettlementBoard = boardWith
    [ nodeOne { building = Just (Settlement playerTwo) }, nodeTwo, nodeThree ]
    [ emptyEdge (EdgeId 1) (NodeId 1, NodeId 2)
    , emptyEdge (EdgeId 2) (NodeId 2, NodeId 3)
    ]

malformedBoard :: Board
malformedBoard = boardWith
    [ malformedNode, nodeTwo ]
    [ emptyEdge (EdgeId 1) (NodeId 1, NodeId 2)
    ]

nodeOne :: Node
nodeOne = emptyNode (NodeId 1) [EdgeId 1]

nodeTwo :: Node
nodeTwo = emptyNode (NodeId 2) [EdgeId 1, EdgeId 2]

nodeThree :: Node
nodeThree = emptyNode (NodeId 3) [EdgeId 2]

malformedNode :: Node
malformedNode = emptyNode (NodeId 99) [EdgeId 1]

emptyNode :: NodeId -> [EdgeId] -> Node
emptyNode nid eids = Node
    { nodeId = nid
    , building = Nothing
    , nodeEdges = eids
    , nodeTiles = []
    }

emptyEdge :: EdgeId -> (NodeId, NodeId) -> Edge
emptyEdge eid ns = Edge
    { edgeId = eid
    , road = Nothing
    , edgeNodes = ns
    }

roadEdge :: EdgeId -> PlayerId -> (NodeId, NodeId) -> Edge
roadEdge eid pid ns = (emptyEdge eid ns) { road = Just (Road pid) }

boardWith :: [Node] -> [Edge] -> Board
boardWith ns es = Board
    { tiles = Map.empty
    , nodes = Map.fromList [ (nodeId node, node) | node <- ns ]
    , edges = Map.fromList [ (edgeId edge, edge) | edge <- es ]
    }
