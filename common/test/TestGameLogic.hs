module TestGameLogic (
      prop_BoardCordHex
    , prop_BoardKeyValues
    , prop_BoardTileAmount
    , prop_BoardNodeAmount
    , prop_BoardEdgeAmount
) where

import Data.Maybe
import Data.Map as Map
import Data.Set as Set (size)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Types
import Coordinates

{-

Test that the logical structure of Catan rules holds up.
This includes testing board layout and data consistency.

-}

-- common function structure in this file
class Eq a => InBoard a where
    inBoard :: Board -> a -> Bool
    allBoard :: Board -> [a] -> Bool

    allBoard b as = and $ Prelude.map (inBoard b) as

instance InBoard TileId where
    inBoard b a = a `elem` (Prelude.map tileId (elems (tiles b))) -- Fix later

instance InBoard NodeId where
    inBoard b a = a `member` (nodes b)

instance InBoard EdgeId where
    inBoard b a = a `member` (edges b)

------------------------------------------------------------
----------------- UNIT TESTING -----------------------------
------------------------------------------------------------

-- Validate Cord

validateCord :: Cord -> Bool
validateCord (Cord q r s) = q + r + s == 0 

validateMapCords :: Map Cord a -> Bool
validateMapCords = and . keys . mapKeys validateCord

-- Validate turn color

validateColorTurn :: GameState -> Bool
validateColorTurn gs = (currentTurn gs) `member` (players gs)

-- Validate the board ids correspond to correct nodes

validateBoardKeyValues :: Board -> Bool
validateBoardKeyValues b = validateBoardKeyValuesNodes (nodes b) && validateBoardKeyValuesEdges (edges b)

validateBoardKeyValuesNodes :: Map NodeId Node -> Bool
validateBoardKeyValuesNodes m = and $ Prelude.map (\(idn, n) -> idn == nodeId n) (toList m)

validateBoardKeyValuesEdges :: Map EdgeId Edge -> Bool
validateBoardKeyValuesEdges m = and $ Prelude.map (\(ide, e) -> ide == edgeId e) (toList m)


-- Validate connections between ids as real ids contained in the board

validatePlayerIDs :: Board -> Player -> Bool
validatePlayerIDs b p = validatePlayerNodeIDs b p && validatePlayerEdgeIDs b p

validatePlayerNodeIDs :: Board -> Player -> Bool
validatePlayerNodeIDs b = allBoard b . buildings

validatePlayerEdgeIDs :: Board -> Player -> Bool
validatePlayerEdgeIDs b = allBoard b . roads

validateTileIDs :: Board -> Tile -> Bool
validateTileIDs b t = validateTileNodeIDs b t && validateTileEdgeIDs b t

validateTileNodeIDs :: Board -> Tile -> Bool
validateTileNodeIDs b = allBoard b . tileNodes

validateTileEdgeIDs :: Board -> Tile -> Bool
validateTileEdgeIDs b = allBoard b . tileEdges

validateNodeIDs :: Board -> Node -> Bool
validateNodeIDs b t = validateNodeEdgeIDs b t && validateNodeTileIDs b t

validateNodeEdgeIDs :: Board -> Node -> Bool
validateNodeEdgeIDs b = allBoard b . nodeEdges

validateNodeTileIDs :: Board -> Node -> Bool
validateNodeTileIDs b = allBoard b . nodeTiles

validateEdgeIDs :: Board -> Edge -> Bool
validateEdgeIDs b e = let (n1, n2) = edgeNodes e in
    inBoard b n1 && inBoard b n2

-- Validate for one desert per board
validateDesertAmount :: Map Cord Tile -> Bool
validateDesertAmount m = Map.foldr (\a b -> b + boolToInt (isDesert a)) 0 m == 1
    where
        isDesert = isNothing . resource
        boolToInt b = case b of 
            True -> 1 
            False -> 0

------------------------------------------------------------
----------------- PROPERTY TESTING -------------------------
------------------------------------------------------------

-- Testing board generation
-- (May come soon)

prop_BoardCordHex :: Board -> Bool
prop_BoardCordHex b = validateMapCords (tiles b)

prop_BoardKeyValues :: Board -> Bool
prop_BoardKeyValues = validateBoardKeyValues

prop_BoardTileAmount :: Board -> Bool
prop_BoardTileAmount b = Map.size (tiles b) == 19

prop_BoardNodeAmount :: Board -> Bool
prop_BoardNodeAmount b = Map.size (nodes b) == 54

prop_BoardEdgeAmount :: Board -> Bool
prop_BoardEdgeAmount b = Map.size (edges b) == 72