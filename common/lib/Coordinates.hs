module Coordinates where

-- Libs
import qualified Data.Map as Map 

-- Local
import Types

{- 
!!! Important !!!
Edges edgeNodes pair contain stub nodes meaning that edge → node → edges is not possible 
and the loopUpTable "nodeEdgeMap" needs to be used with the nodeId for tranversal 
-}

-- Every valid hex satisfies q + r + s = 0
-- hexRange n generates all cube coords within radius n of the origin
hexRange :: Int -> [Cord]
hexRange n =
    [ Cord q r (-q-r) | q <- [-n..n], r <- [max (-n) (-q-n) .. min n (-q+n)]
    ]

-- The 19 tile positions of a standard Catan board (radius 2)
catanCords :: [Cord]
catanCords = hexRange 2

addCord :: Cord -> Cord -> Cord
addCord (Cord q1 r1 s1) (Cord q2 r2 s2) = Cord (q1+q2) (r1+r2) (s1+s2)

mkCord :: Int -> Int -> Int -> Maybe Cord
mkCord q r s
    | q + r + s == 0 = Just (Cord q r s)
    | otherwise      = Nothing

distance :: Cord -> Cord -> Int
distance (Cord q1 r1 s1) (Cord q2 r2 s2) =
    maximum [abs (q1-q2), abs (r1-r2), abs (s1-s2)]

inBounds :: Cord -> Bool
inBounds cord@(Cord q r s) = q + r + s == 0 && distance origin cord <= 2
  where
    origin = Cord 0 0 0

-- All surrounding directions to a specific tile
directions :: [Cord]
directions =
    [ Cord  1 (-1)  0
    , Cord  1   0  (-1)
    , Cord  0   1  (-1)
    , Cord (-1)  1   0
    , Cord (-1)  0   1
    , Cord  0  (-1)  1
    ]

neighbours :: Cord -> [Cord]
neighbours c = map (addCord c) directions

-- Standard resource distribution
-- fixed layout, reading from top-left to bottom-right

tileData :: [(Maybe Resource, Int)]
tileData =
    -- 3 tiles, top
    [ (Just Ore,   10)
    , (Just Wool,    2)
    , (Just Lumber,     9)
    -- 4 tiles
    , (Just Grain,   12)
    , (Just Brick,     6)
    , (Just Wool,    4)
    , (Just Brick,    10)
    -- 5 tiles, middle
    , (Just Grain,    9)
    , (Just Lumber,    11)
    , (Nothing,       0)   -- Desert
    , (Just Lumber,     3)
    , (Just Ore,    8)
    -- 4 tiles
    , (Just Lumber,     8)
    , (Just Ore,    3)
    , (Just Grain,    4)
    , (Just Wool,    5)
    -- 3 tiles, bottom
    , (Just Brick,     5)
    , (Just Grain,    6)
    , (Just Wool,   11)
    ]

makeTile :: Int -> (Maybe Resource, Int) -> (Cord, Tile)
makeTile idx (res, tok) =
    ( catanCords !! idx
    , Tile
        { tileId  = idx
        , resource = res
        , token    = tok
        , robber   = False  -- robber
        , nodes    = []
        , edges    = []        
        }
    )

catanTiles :: [(Cord, Tile)]
catanTiles = zipWith makeTile [0..] tileData

makeNode :: Int -> Maybe Building -> [Edge] -> [Int] -> Node
makeNode nid bld nE nT = Node
  { nodeId = NodeId nid
  , building = bld
  , nodeEdges = nE
  , nodeTiles = nT
  } 

makeEdge :: Int -> Maybe Road -> (Node, Node) -> Edge 
makeEdge eid rd nds = Edge 
  { edgeId = EdgeId eid 
  , road = rd
  , edgeNodes = nds
  }

-- Building node and edge pairs
-- Pass 1: build all 72 edges with placeholder nodes
-- Nodes inside Edge are minimal — just ID, no edges/tiles yet
buildEdges :: Map.Map Int Edge
buildEdges = Map.fromList
    [ (eid, makeEdge eid Nothing (stubNode n1, stubNode n2)) 
    | (eid, (NodeId n1, NodeId n2)) <- zip [1..] edgeNodePairs]
  where
    stubNode i = makeNode i Nothing [] []

-- Pass 2: build all 54 nodes using real edges and tileIds
buildNodes :: Map.Map Int Edge -> Map.Map Int Node
buildNodes edgeMap = Map.fromList 
    [ (nid, makeNode nid Nothing myEdges myTiles)
    | nid <- [1..54]
    , let myEdgeIds = nodeEdgeMap Map.! nid 
          myEdges   = [ edgeMap Map.! eid | eid <- myEdgeIds ]
          myTiles   = Map.findWithDefault [] nid nodeTileMap
    ]

-- Pass 3: populate tiles with proper nodes and edges
buildBoard :: [(Cord, Tile)]
buildBoard =
    let edgeMap = buildEdges
        nodeMap = buildNodes edgeMap
        lookupNode i  = nodeMap Map.! i
        lookupEdge i  = edgeMap Map.! i
    in  [ ( cord
          , tile
              { nodes = map lookupNode (tileNodes !! tileId tile)
              , edges = map lookupEdge (tileEdges !! tileId tile)
              }
          )
        | (cord, tile) <- catanTiles
        ]

catanBoard :: Board
catanBoard = Board $ Map.fromList buildBoard


tileNodes :: [[Int]]
tileNodes =
  -- Row 1 (3 tiles)
  [ [1,5,9,13,8,4]       
  , [2,6,10,14,9,5]      
  , [3,7,11,15,10,6]     
  -- Row 2 (4 tiles)
  , [8,13,18,23,17,12]   
  , [9,14,19,24,18,13]   
  , [10,15,20,25,19,14]  
  , [11,16,21,26,20,15]  
  -- Row 3 (5 tiles)
  , [17,23,29,34,28,22]  
  , [18,24,30,35,29,23]  
  , [19,25,31,36,30,24]  
  , [20,26,32,37,31,25]  
  , [21,27,33,38,32,26]  
  -- Row 4 (4 tiles)
  , [29,35,40,44,39,34]  
  , [30,36,41,45,40,35]  
  , [31,37,42,46,41,36]  
  , [32,38,43,47,42,37]  
  -- Row 5 (3 tiles)
  , [40,45,49,52,48,44]  
  , [41,46,50,53,49,45]  
  , [42,47,51,54,50,46]  
  ]

tileEdges :: [[Int]]
tileEdges =
  -- Row 1 (3 tiles)
  [ [2,8,13,12,7,1]       
  , [4,9,15,14,8,3]       
  , [6,10,17,16,9,5]      
  -- Row 2 (4 tiles)
  , [12,20,26,25,19,11]   
  , [14,21,28,27,20,13]   
  , [16,22,30,29,21,15]   
  , [18,23,32,31,22,17]   
  -- Row 3 (5 tiles)
  , [25,35,41,40,34,24]   
  , [27,36,43,42,35,26]   
  , [29,37,45,44,36,28]   
  , [31,38,47,46,37,30]   
  , [33,39,49,48,38,32]   
  -- Row 4 (4 tiles)
  , [42,51,56,55,50,41]   
  , [44,52,58,57,51,43]   
  , [46,53,60,59,52,45]   
  , [48,54,62,61,53,47]   
  -- Row 5 (3 tiles)
  , [57,64,68,67,63,56]   
  , [59,65,70,69,64,58]   
  , [61,66,72,71,65,60]   
  ]


-- The 2-3 edge ids for each node, lookup on nodeId  
nodeEdgeMap :: Map.Map Int [Int]
nodeEdgeMap = Map.fromList
    [ (1,  [1,2])        
    , (2,  [3,4])        
    , (3,  [5,6])        
    , (4,  [1,7])        
    , (5,  [2,3,8])      
    , (6,  [4,5,9])      
    , (7,  [6,10])       
    , (8,  [7,11,12])    
    , (9,  [8,13,14])    
    , (10, [9,15,16])    
    , (11, [10,17,18])   
    , (12, [11,19])      
    , (13, [12,13,20])   
    , (14, [14,15,21])   
    , (15, [16,17,22])   
    , (16, [18,23])      
    , (17, [19,24,25])   
    , (18, [20,26,27])   
    , (19, [21,28,29])   
    , (20, [22,30,31])   
    , (21, [23,32,33])   
    , (22, [24,34])      
    , (23, [25,26,35])   
    , (24, [27,28,36])   
    , (25, [29,30,37])   
    , (26, [31,32,38])   
    , (27, [33,39])      
    , (28, [34,40])      
    , (29, [35,41,42])   
    , (30, [36,43,44])   
    , (31, [37,45,46])   
    , (32, [38,47,48])   
    , (33, [39,49])      
    , (34, [40,41,50])   
    , (35, [42,43,51])   
    , (36, [44,45,52])   
    , (37, [46,47,53])   
    , (38, [48,49,54])   
    , (39, [50,55])      
    , (40, [51,56,57])   
    , (41, [52,58,59])   
    , (42, [53,60,61])   
    , (43, [54,62])      
    , (44, [55,56,63])   
    , (45, [57,58,64])   
    , (46, [59,60,65])   
    , (47, [61,62,66])   
    , (48, [63,67])      
    , (49, [64,68,69])   
    , (50, [65,70,71])   
    , (51, [66,72])      
    , (52, [67,68])      
    , (53, [69,70])      
    , (54, [71,72])      
    ]

-- nodePair for each edge, lookup on edgeId 
edgeNodeMap :: Map.Map Int (NodeId, NodeId)
edgeNodeMap = Map.fromList
    [ (eid, (n1, n2))
    | (eid, (n1, n2)) <- zip [1..] edgeNodePairs
    ]

-- The two nodes each edge connects, in edge-number order 1..72
edgeNodePairs :: [(NodeId, NodeId)]
edgeNodePairs =
    [ (NodeId 4,  NodeId 1) 
    , (NodeId 1,  NodeId 5) 
    , (NodeId 5,  NodeId 2) 
    , (NodeId 2,  NodeId 6) 
    , (NodeId 6,  NodeId 3) 
    , (NodeId 3,  NodeId 7) 
    , (NodeId 8,  NodeId 4) 
    , (NodeId 5,  NodeId 9) 
    , (NodeId 6,  NodeId 10)
    , (NodeId 7,  NodeId 11)
    , (NodeId 12, NodeId 8) 
    , (NodeId 13, NodeId 8) 
    , (NodeId 9,  NodeId 13)
    , (NodeId 14, NodeId 9) 
    , (NodeId 10, NodeId 14)
    , (NodeId 15, NodeId 10)
    , (NodeId 11, NodeId 15)
    , (NodeId 11, NodeId 16)
    , (NodeId 12, NodeId 17)
    , (NodeId 13, NodeId 18)
    , (NodeId 14, NodeId 19)
    , (NodeId 15, NodeId 20)
    , (NodeId 16, NodeId 21)
    , (NodeId 22, NodeId 17)
    , (NodeId 23, NodeId 17)
    , (NodeId 18, NodeId 23)
    , (NodeId 24, NodeId 18)
    , (NodeId 19, NodeId 24)
    , (NodeId 25, NodeId 19)
    , (NodeId 20, NodeId 25)
    , (NodeId 26, NodeId 20)
    , (NodeId 21, NodeId 26)
    , (NodeId 21, NodeId 27)
    , (NodeId 22, NodeId 28)
    , (NodeId 23, NodeId 29)
    , (NodeId 24, NodeId 30)
    , (NodeId 25, NodeId 31)
    , (NodeId 26, NodeId 32)
    , (NodeId 27, NodeId 33)
    , (NodeId 34, NodeId 28)
    , (NodeId 29, NodeId 34)
    , (NodeId 35, NodeId 29)
    , (NodeId 30, NodeId 35)
    , (NodeId 36, NodeId 30)
    , (NodeId 31, NodeId 36)
    , (NodeId 37, NodeId 31)
    , (NodeId 32, NodeId 37)
    , (NodeId 38, NodeId 32)
    , (NodeId 33, NodeId 38)
    , (NodeId 34, NodeId 39)
    , (NodeId 35, NodeId 40)
    , (NodeId 36, NodeId 41)
    , (NodeId 37, NodeId 42)
    , (NodeId 38, NodeId 43)
    , (NodeId 44, NodeId 39)
    , (NodeId 40, NodeId 44)
    , (NodeId 45, NodeId 40)
    , (NodeId 41, NodeId 45)
    , (NodeId 46, NodeId 41)
    , (NodeId 42, NodeId 46)
    , (NodeId 47, NodeId 42)
    , (NodeId 43, NodeId 47)
    , (NodeId 44, NodeId 48)
    , (NodeId 45, NodeId 49)
    , (NodeId 46, NodeId 50)
    , (NodeId 47, NodeId 51)
    , (NodeId 52, NodeId 48)
    , (NodeId 49, NodeId 52)
    , (NodeId 53, NodeId 49)
    , (NodeId 50, NodeId 53)
    , (NodeId 54, NodeId 50)
    , (NodeId 51, NodeId 54)
    ]

-- Maps each NodeId to which tileIds contain it
nodeTileMap :: Map.Map Int [Int]
nodeTileMap = Map.fromList
    [ (1,  [0])          -- tile: t0
    , (2,  [1])          -- tile: t1
    , (3,  [2])          -- tile: t2
    , (4,  [0])          -- tile: t0
    , (5,  [0,1])        -- tiles: t0,t1
    , (6,  [1,2])        -- tiles: t1,t2
    , (7,  [2])          -- tile: t2
    , (8,  [0,3])        -- tiles: t0,t3
    , (9,  [0,1,4])      -- tiles: t0,t1,t4
    , (10, [1,2,5])      -- tiles: t1,t2,t5
    , (11, [2,6])        -- tiles: t2,t6
    , (12, [3])          -- tile: t3
    , (13, [0,3,4])      -- tiles: t0,t3,t4
    , (14, [1,4,5])      -- tiles: t1,t4,t5
    , (15, [2,5,6])      -- tiles: t2,t5,t6
    , (16, [6])          -- tile: t6
    , (17, [3,7])        -- tiles: t3,t7
    , (18, [3,4,8])      -- tiles: t3,t4,t8
    , (19, [4,5,9])      -- tiles: t4,t5,t9
    , (20, [5,6,10])     -- tiles: t5,t6,t10
    , (21, [6,11])       -- tiles: t6,t11
    , (22, [7])          -- tile: t7
    , (23, [3,7,8])      -- tiles: t3,t7,t8
    , (24, [4,8,9])      -- tiles: t4,t8,t9
    , (25, [5,9,10])     -- tiles: t5,t9,t10
    , (26, [6,10,11])    -- tiles: t6,t10,t11
    , (27, [11])         -- tile: t11
    , (28, [7])          -- tile: t7
    , (29, [7,8,12])     -- tiles: t7,t8,t12
    , (30, [8,9,13])     -- tiles: t8,t9,t13
    , (31, [9,10,14])    -- tiles: t9,t10,t14
    , (32, [10,11,15])   -- tiles: t10,t11,t15
    , (33, [11])         -- tile: t11
    , (34, [7,12])       -- tiles: t7,t12
    , (35, [8,12,13])    -- tiles: t8,t12,t13
    , (36, [9,13,14])    -- tiles: t9,t13,t14
    , (37, [10,14,15])   -- tiles: t10,t14,t15
    , (38, [11,15])      -- tiles: t11,t15
    , (39, [12])         -- tile: t12
    , (40, [12,13,16])   -- tiles: t12,t13,t16
    , (41, [13,14,17])   -- tiles: t13,t14,t17
    , (42, [14,15,18])   -- tiles: t14,t15,t18
    , (43, [15])         -- tile: t15
    , (44, [12,16])      -- tiles: t12,t16
    , (45, [13,16,17])   -- tiles: t13,t16,t17
    , (46, [14,17,18])   -- tiles: t14,t17,t18
    , (47, [15,18])      -- tiles: t15,t18
    , (48, [16])         -- tile: t16
    , (49, [16,17])      -- tiles: t16,t17
    , (50, [17,18])      -- tiles: t17,t18
    , (51, [18])         -- tile: t18
    , (52, [16])         -- tile: t16
    , (53, [17])         -- tile: t17
    , (54, [18])         -- tile: t18
    ]