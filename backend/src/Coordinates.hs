module Coordinates where
import Types
import Data.Map()

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
        , robber   = res == Nothing   -- robber
        , nodes    = []               
        }
    )

catanTiles :: [(Cord, Tile)]
catanTiles = zipWith makeTile [0..] tileData
