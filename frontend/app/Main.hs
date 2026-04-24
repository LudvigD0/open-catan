----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}

{-# LANGUAGE TemplateHaskell #-}


----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import           Miso.Lens

import qualified Miso.Svg.Element as S
import qualified Miso.Svg.Property as SP
import qualified Miso.CSS as CSS
import           Miso.String (ms)
import           Data.List (intercalate)

{- import qualified Miso.Html.Element as H
 -}



----------------------------------------------------------------------------
-- | Sum type for App events
data Action
  = NoOp
  | ClickHex
  deriving (Show, Eq)
----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = startApp defaultEvents app 
  
----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
-- | `component` takes as arguments the initial model, update function, view function
app :: App () Action
app = component () updateModel viewModel


----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Effect parent () Action
updateModel NoOp = pure ()


--------------------------- helpers
data Hex = Hex Int Int Int
  deriving (Show, Eq, Ord)

boardHexes :: [Hex]
boardHexes = -- skapar 19 hex
  [ Hex q r s
  | q <- [-2..2]
  , r <- [-2..2]
  , s <- [-2..2]
  , q + r + s == 0
  ]

hexToPixel :: Double -> Hex -> (Double, Double) -- beräkna mittpunkten
hexToPixel size (Hex q r _s) =
  let q' = fromIntegral q
      r' = fromIntegral r
      x = size * sqrt 3 * (q' + r' / 2)
      y = size * 1.5 * r'
  in (x + 300, y + 300)

hexCorners :: Double -> (Double, Double) -> [(Double, Double)]
hexCorners size (cx, cy) =
  [ let angle = pi / 180 * (60 * fromIntegral i - 30)
    in (cx + size * cos angle, cy + size * sin angle)
  | i <- [0..5]
  ]

pointsText :: [(Double, Double)] -> MisoString -- 
pointsText points =
  ms $ intercalate " "
    [ show x <> "," <> show y
    | (x, y) <- points
    ]

viewHex :: Hex -> View () Action
viewHex hex =
  let center = hexToPixel 60 hex --beräkna mittpunkt
      points = hexCorners 60 center -- skicka mittpunkten för att beräkna alla 6 kordinater
  in
    S.polygon_
      [ SP.points_ (pointsText points) -- Kordinater går från tuples till läsbar syntax fär miso vilket är ex: "100,100"
      , CSS.style_
        [ CSS.fill "#d9a066"
        , CSS.stroke "black"
        , CSS.strokeWidth "3"
        ]
      ]

------------------------------helpers end
----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model



viewModel :: () -> View () Action
viewModel _ =
  H.div_
    []
    [ H.svg_
      [ SP.viewBox_ "0 0 600 600"
      , CSS.style_
        [ CSS.width "600px"
        , CSS.height "600px"
        , CSS.borderStyle "solid"
        ]
      ]
      (map viewHex boardHexes)
    ]



     {-  [ S.polygon_
        [ SP.points_ "250,100 380,175 380,325 250,400 120,325 120,175"
        , CSS.style_
          [ CSS.fill "#d9a066"
          , CSS.stroke "black"
          , CSS.strokeWidth "4"
          ]
        ]
      ] -}

{- 
viewModel :: Int -> View Int Action
viewModel x =
  H.div_
    [ P.className "counter"
    ]
    [ H.button_ [ H.onClick AddOne ] [ text "+" ]
    , text (ms x)
    , H.button_ [ H.onClick SubtractOne ] [ text "-" ]
    , H.br_ []
    , H.button_ [ H.onClick SayHelloWorld ] [ text "Alert Hello World!" ]
    ] -}
----------------------------------------------------------------------------