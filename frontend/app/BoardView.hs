{-# LANGUAGE OverloadedStrings #-}


module BoardView where

import           Miso
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as HP
import qualified Miso.CSS as CSS
import           Miso.String (ms)
import           Miso.Html.Property (className)
import Miso.Html.Event (onClick)

import qualified Data.Map as Map

import Types
import Coordinates
  ( catanCords
  , tileNodeIds
  )


{- boardHexes :: [Cord] 
boardHexes = -- skapar 19 hex
  [ Cord q r s
  | q <- [-2..2]
  , r <- [-2..2]
  , s <- [-2..2]
  , q + r + s == 0
  ] -}


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
           (\nodeId offset -> (nodeId, addPixel center offset))
           nodeIds
           corners
    | (cord, nodeIds) <- zip visualCatanCords tileNodeIds -- catanCords
    ]

uniqueNodePositions :: Double -> [(Int, (Double, Double))]
uniqueNodePositions size =
  Map.toList $ Map.fromList (tileNodePositions size)


viewNode :: (Int, (Double, Double)) -> View () Action
viewNode (nid, (x, y)) =
  H.div_
    [ CSS.style_
        [ CSS.position "absolute"
        , CSS.left (ms (show (x - 10) ++ "px"))
        , CSS.top   (ms (show (y - 10) ++ "px"))
        , CSS.width "20px"
        , CSS.height "20px"
        , CSS.borderRadius "50%"
        , CSS.cursor "pointer"
        , CSS.backgroundColor (CSS.Hex "ff5454")
        , CSS.zIndex "10"
        ]
      , onClick (ClickNode (NodeId nid))
    ]
    []
  

viewHex :: Cord -> View () Action
viewHex hex =
  let xy = hexTopLeft 80 hex -- xy kordinaten längst upp till vänster
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
      [
        H.img_
        [ HP.src_ "/static/desert.png"
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
      ]

    


viewBoard :: View () Action
viewBoard =
  let size = 80
      hexViews  = map viewHex visualCatanCords  --catanCords
      nodeViews = map viewNode (uniqueNodePositions size)
  in
    H.div_
      [ className "wrapper" ]
      (hexViews ++ nodeViews)