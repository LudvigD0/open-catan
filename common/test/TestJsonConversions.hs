module TestJsonConversions (
  prop_GameActionConversion,
  prop_CordConversion,
  prop_BoardConversion,
  prop_ColorConversion,
  prop_ResourceConversion,
  prop_RoadConversion,
  prop_BuildingConversion,
  prop_PlayerIdConversion,
  prop_PlayerConversion,
  prop_GameStateConversion,
  prop_TurnPhaseConversion,
  prop_WSMessageConversion,
  prop_TileIdConversion,
  prop_EdgeIdConversion,
  prop_NodeIdConversion,
  prop_TileConversion,
  prop_EdgeConversion,
  prop_NodeConversion
  ) where

{-
  # Ensure that our serializable types can be smoothly converted
  # back and forth over Json.
  
  Not sure how necessary these tests will be, 
  but I believe it's better to have them around, and potentially improve them.

  P.S. I could not extract a helper function because a fully polymorphic function
  for (Json.decode . Json.encode) is apparently impossible,
-}

import qualified Data.Aeson as Json
import                         Types
  ( Cord
  , Board
  , Color
  , Resource
  , Road
  , Building
  , PlayerId
  , Player
  , GameAction
  , GameState
  , TurnPhase
  , WSMessage
  , TileId
  , EdgeId
  , NodeId
  , Tile
  , Edge
  , Node
  )


prop_GameActionConversion :: GameAction -> Bool
prop_GameActionConversion x = maybe False (== x) $ (Json.decode . Json.encode) x

prop_CordConversion :: Cord -> Bool
prop_CordConversion x = maybe False (== x) $ (Json.decode . Json.encode) x

prop_BoardConversion :: Board -> Bool
prop_BoardConversion x = maybe False (== x) $ (Json.decode . Json.encode) x

prop_ColorConversion :: Color -> Bool
prop_ColorConversion x = maybe False (== x) $ (Json.decode . Json.encode) x

prop_ResourceConversion :: Resource -> Bool
prop_ResourceConversion x = maybe False (== x) $ (Json.decode . Json.encode) x

prop_RoadConversion :: Road -> Bool
prop_RoadConversion x = maybe False (== x) $ (Json.decode . Json.encode) x

prop_BuildingConversion :: Building -> Bool
prop_BuildingConversion x = maybe False (== x) $ (Json.decode . Json.encode) x

prop_PlayerIdConversion :: PlayerId -> Bool
prop_PlayerIdConversion x = maybe False (== x) $ (Json.decode . Json.encode) x

prop_PlayerConversion :: Player -> Bool
prop_PlayerConversion x = maybe False (== x) $ (Json.decode . Json.encode) x

prop_GameStateConversion :: GameState -> Bool
prop_GameStateConversion x = maybe False (== x) $ (Json.decode . Json.encode) x

prop_TurnPhaseConversion :: TurnPhase -> Bool
prop_TurnPhaseConversion x = maybe False (== x) $ (Json.decode . Json.encode) x

prop_WSMessageConversion :: WSMessage -> Bool
prop_WSMessageConversion x = maybe False (== x) $ (Json.decode . Json.encode) x

prop_TileIdConversion :: TileId -> Bool
prop_TileIdConversion x = maybe False (== x) $ (Json.decode . Json.encode) x

prop_EdgeIdConversion :: EdgeId -> Bool
prop_EdgeIdConversion x = maybe False (== x) $ (Json.decode . Json.encode) x

prop_NodeIdConversion :: NodeId -> Bool
prop_NodeIdConversion x = maybe False (== x) $ (Json.decode . Json.encode) x

prop_TileConversion :: Tile -> Bool
prop_TileConversion x = maybe False (== x) $ (Json.decode . Json.encode) x

prop_EdgeConversion :: Edge -> Bool
prop_EdgeConversion x = maybe False (== x) $ (Json.decode . Json.encode) x

prop_NodeConversion :: Node -> Bool
prop_NodeConversion x = maybe False (== x) $ (Json.decode . Json.encode) x

---
