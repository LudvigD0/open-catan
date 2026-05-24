{-# LANGUAGE OverloadedStrings #-}


module BoardView where

import           Miso
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as HP
import qualified Miso.CSS as CSS
import           Miso.Html.Property (className)
import Miso.Html.Event (onClick)

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import ClientTypes

import Types
import Coordinates
  ( edgeNodeMap
  , tileNodeIds
  )



-----Helper functions

nodeIdToInt :: NodeId -> Int
nodeIdToInt (NodeId n) = n

px :: Double -> MisoString
px n = ms (show n ++ "px")


edgeImageRatio :: Double
edgeImageRatio = 130 / 620

assetColorName :: Color -> String
assetColorName Red    = "red"
assetColorName Blue   = "blue"
assetColorName Orange = "orange"
assetColorName White  = "white"

houseImage :: Color -> MisoString
houseImage color = ms ("/static/houses/house-" ++ assetColorName color ++ ".png")

cityImage :: Color -> MisoString
cityImage color = ms ("/static/cities/city-" ++ assetColorName color ++ ".png")

edgeImage :: Color -> MisoString
edgeImage color = ms ("/static/edges/edge-" ++ assetColorName color ++ ".png")

tileImage :: Tile -> MisoString
tileImage tile =
  case resource tile of
    Nothing     -> "/static/desert.png"
    Just Lumber -> "/static/wood.png"
    Just Ore    -> "/static/ore.png"
    Just Grain  -> "/static/wheat.png"
    Just Brick  -> "/static/brick.png"
    Just Wool   -> "/static/sheep.png"

defaultHouseImage :: MisoString
defaultHouseImage = ms ("/static/houses/house-white.png" :: String)

defaultCityImage :: MisoString
defaultCityImage = ms ("/static/cities/city-white.png" :: String)

defaultEdgeImage :: MisoString
defaultEdgeImage = ms ("/static/edges/edge-white.png" :: String)

playerColor :: GameState -> PlayerId -> Maybe Color
playerColor gs pid =
  fst <$> findPlayer (Map.toList (players gs))
  where
    findPlayer [] = Nothing
    findPlayer ((color, player) : rest)
      | playerId player == pid = Just (color, player)
      | otherwise = findPlayer rest

----



edgePositions :: Double -> [(Int, ((Double, Double), (Double, Double)))]
edgePositions size =
  let nodePosMap = Map.fromList (uniqueNodePositions size)
  in
    [ ( eid
      , ( nodePosMap Map.! nodeIdToInt n1
        , nodePosMap Map.! nodeIdToInt n2
        )
      )
    | (eid, (n1, n2)) <- Map.toList edgeNodeMap
    ]



visualCatanCords :: [Cord]
visualCatanCords =
  [ Cord 0 (-2) 2, Cord 1 (-2) 1, Cord 2 (-2) 0
  , Cord (-1) (-1) 2, Cord 0 (-1) 1, Cord 1 (-1) 0, Cord 2 (-1) (-1)
  , Cord (-2) 0 2, Cord (-1) 0 1, Cord 0 0 0, Cord 1 0 (-1), Cord 2 0 (-2)
  , Cord (-2) 1 1, Cord (-1) 1 0, Cord 0 1 (-1), Cord 1 1 (-2)
  , Cord (-2) 2 0, Cord (-1) 2 (-1), Cord 0 2 (-2)
  ]

hexCenter :: Double -> Cord -> (Double, Double)
hexCenter size (Cord q r _s) =
  let q' = fromIntegral q
      r' = fromIntegral r
      x = size * sqrt 3 * (q' + r' / 2)
      y = size * 1.5 * r'
  in (x, y)

hexTopLeft :: Double -> Cord -> (Double, Double)
hexTopLeft size cord =
  let (cx, cy) = hexCenter size cord
      width  = sqrt 3 * size
      height = 2 * size
  in (cx - width / 2, cy - height / 2)



hexCornerOffsets :: Double -> [(Double, Double)]
hexCornerOffsets size =
  [ (0, -size)
  , (sqrt 3 / 2 * size, -size / 2)
  , (sqrt 3 / 2 * size,  size / 2)
  , (0, size)
  , (-sqrt 3 / 2 * size, size / 2)
  , (-sqrt 3 / 2 * size, -size / 2)
  ]



addPixel :: (Double, Double) -> (Double, Double) -> (Double, Double)
addPixel (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


tileNodePositions :: Double -> [(Int, (Double, Double))]
tileNodePositions size =
  concat
    [ let center = hexCenter size cord
          corners = hexCornerOffsets size
      in zipWith
           (\nid cornerOffset -> (nid, addPixel center cornerOffset))
           nodeIds
           corners
    | (cord, nodeIds) <- zip visualCatanCords tileNodeIds -- catanCords
    ]

uniqueNodePositions :: Double -> [(Int, (Double, Double))]
uniqueNodePositions size =
  Map.toList $ Map.fromList (tileNodePositions size)

-- Klickbar plats for en node.
viewNodeSlot :: (Int, (Double, Double)) -> View Model Action
viewNodeSlot (nid, (x, y)) =
  H.div_
    [ className "clickable-node"
    , CSS.style_
        [ CSS.position "absolute"
        , CSS.left (px (x - 10))
        , CSS.top  (px (y - 10))
        , CSS.width "20px"
        , CSS.height "20px"
        , CSS.borderRadius "50%"
        , CSS.cursor "pointer"
        , CSS.border "1px solid rgba(255,255,255,0.55)"
        , CSS.zIndex "20"
        ]
      , onClick (ClickNode (NodeId nid))
    ]
    []

-- Ett hus eller city från gamestate
viewBuilding :: GameState -> (Int, (Double, Double)) -> Maybe (View Model Action)
viewBuilding gs (nid, (x, y)) = do
  boardNode <- Map.lookup (NodeId nid) (nodes (board gs))
  buildingView <$> building boardNode
  where
    buildingView b =
      let (size, imageSrc) =
            case b of
              Settlement owner ->
                (40, maybe defaultHouseImage houseImage (playerColor gs owner))
              City owner ->
                (52, maybe defaultCityImage cityImage (playerColor gs owner))
      in
        H.img_
          [ HP.src_ imageSrc
          , HP.alt_ "Building"
          , CSS.style_
              [ CSS.position "absolute"
              , CSS.left (px (x - size / 2))
              , CSS.top  (px (y - size / 2))
              , CSS.width (px size)
              , CSS.height (px size)
              , CSS.zIndex "30"
              , CSS.cursor "pointer"
              ]
          , onClick (ClickNode (NodeId nid))
          ]


-- Klickbar plats for en edge
viewEdgeSlot :: (Int, ((Double, Double), (Double, Double))) -> View Model Action
viewEdgeSlot (eid, endpoints) =
  viewEdgeShape endpoints 18 "15" (ClickEdge (EdgeId eid))

-- En riktig väg från gamestate
viewRoad :: GameState -> (Int, ((Double, Double), (Double, Double))) -> Maybe (View Model Action)
viewRoad gs (eid, endpoints) = do
  edge <- Map.lookup (EdgeId eid) (edges (board gs))
  Road owner <- road edge
  let imageSrc = maybe defaultEdgeImage edgeImage (playerColor gs owner)
  pure (viewRoadImage endpoints imageSrc "25" (ClickEdge (EdgeId eid)))

viewRoadImage ::
  ((Double, Double), (Double, Double)) ->
  MisoString ->
  MisoString ->
  Action ->
  View Model Action
viewRoadImage ((x1, y1), (x2, y2)) imageSrc z action =
  let dx = x2 - x1
      dy = y2 - y1

      midX = (x1 + x2) / 2
      midY = (y1 + y2) / 2

      edgeLength = sqrt (dx * dx + dy * dy)
      roadWidth = edgeLength * 0.75
      roadHeight = roadWidth * edgeImageRatio

      angle = atan2 dy dx
  in
    H.img_
      [ HP.src_ imageSrc
      , HP.alt_ "Road"
      , CSS.style_
          [ CSS.position "absolute"
          , CSS.left (px (midX - roadWidth / 2))
          , CSS.top  (px (midY - roadHeight / 2))
          , CSS.width (px roadWidth)
          , CSS.height (px roadHeight)
          , CSS.cursor "pointer"
          , CSS.transform (ms ("rotate(" ++ show angle ++ "rad)"))
          , CSS.zIndex z
          , CSS.transformOrigin "center"
          ]
      , onClick action
      ]

viewEdgeShape ::
  ((Double, Double), (Double, Double)) ->
  Double ->
  MisoString ->
  Action ->
  View Model Action
viewEdgeShape ((x1, y1), (x2, y2)) height z action =
  let dx = x2 - x1
      dy = y2 - y1

      midX = (x1 + x2) / 2
      midY = (y1 + y2) / 2

      edgeLength = sqrt (dx * dx + dy * dy)

      angle = atan2 dy dx
  in
    H.div_
      [ className "clickable-edge"
      , CSS.style_
          [ CSS.position "absolute"
          , CSS.left (px (midX - edgeLength / 2))
          , CSS.top  (px (midY - height / 2))
          , CSS.width  (px edgeLength)
          , CSS.height (px height)
          , CSS.borderRadius "999px"
          , CSS.cursor "pointer"
          , CSS.transform (ms ("rotate(" ++ show angle ++ "rad)"))
          , CSS.zIndex z
          , CSS.transformOrigin "center"
          ]
      , onClick action
      ]
      []
  

viewHex :: (Cord, Tile) -> View Model Action
viewHex (hex, tile) =
  let xy = hexTopLeft 80 hex -- xy kordinaten längst upp till vänster
      tokenView =
        if token tile == 0
          then []
          else
            [ H.div_
                [ className "tile-token" ]
                [ text (ms (show (token tile))) ]
            ]
  in
    H.div_
      [ CSS.style_ 
        [
          CSS.left (ms (show (fst xy) ++ "px"))
          , CSS.top (ms (show (snd xy) ++ "px"))
          , CSS.position "absolute"
          , CSS.width "138.56px"
          , CSS.height "160px"
        ]
      ]
      ([
        H.img_
        [ HP.src_ (tileImage tile)
          , HP.alt_ "Tile image"
          , CSS.style_
            [ CSS.width "129.9px"
            , CSS.height "150px"
            , CSS.position "absolute"
            , CSS.left "50%"
            , CSS.top "50%"
            , CSS.transform "translate(-50%, -50%)"
            ]
        ]
      ] ++ tokenView)
    


viewBoard :: GameState -> View Model Action
viewBoard gs =
  let size = 80
      hexViews = map viewHex (Map.toList (tiles (board gs)))
      edgePos = edgePositions size
      nodePos = uniqueNodePositions size
      edgeSlotViews = map viewEdgeSlot edgePos
      nodeSlotViews = map viewNodeSlot nodePos
      roadViews = mapMaybe (viewRoad gs) edgePos
      buildingViews = mapMaybe (viewBuilding gs) nodePos
  in
    H.div_
      [ className "wrapper" ]
      (hexViews ++ edgeSlotViews ++ nodeSlotViews ++ roadViews ++ buildingViews)





{- boardHexes :: [Cord] 
boardHexes = -- skapar 19 hex
  [ Cord q r s
  | q <- [-2..2]
  , r <- [-2..2]
  , s <- [-2..2]
  , q + r + s == 0
  ] -}
