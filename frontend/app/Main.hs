----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}

{-# LANGUAGE TemplateHaskell #-}


----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------

import Miso
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as HP
import Miso.Html.Property (className, id_)

import BoardView
import Types
import ClientTypes
import Catan (autoPlace, initGameState)

import Data.UUID.Types (UUID, fromWords)
--import Data.UUID (UUID)
--import qualified Data.UUID as UUID





{- import qualified Miso.Html.Element as H
 -}



----------------------------------------------------------------------------
-- | Sum type for App events

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
app :: App Model Action
app = component initialModel updateModel viewModel


----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Effect parent Model Action
updateModel NoOp = pure ()
updateModel ClickHex = pure ()
updateModel (ClickNode _) = pure ()
updateModel (ClickEdge _) = pure ()


--------------------------- helpers
--data Hex = Hex Int Int Int
--  deriving (Show, Eq, Ord)

-- Board hexes ska antagligen bort eftersom att vi redan skapar board i Cordinates




--nodePosition :: 

--edgePosition :: 






uuids :: [UUID]
uuids =
  [ fromWords 0x550e8400 0xe29b41d4 0xa7164466 0x55440000
  , fromWords 0x6ba7b810 0x9dad11d1 0x80b400c0 0x4fd430c8
  , fromWords 0x123e4567 0xe89b12d3 0xa4564266 0x14174000
  , fromWords 0xf47ac10b 0x58cc4372 0xa5670e02 0xb2c3d479
  ]

initialGameState :: GameState
initialGameState =
  autoPlace (initGameState uuids 0)


initialModel :: Model
initialModel = Model
  { gameState = initialGameState
  , selectedNode = Nothing
  , selectedEdge = Nothing
  }

--gameState :: IO GameState
--gameState = initGameState uuids






--- model fungerar som wrappern för allt den ritar ut (body ungefär)
viewModel :: Model -> View Model Action
viewModel appModel =
  H.div_
    [ id_ "container", className "container" ]
    [ viewWater
    , viewSand
    , viewBoard (gameState appModel)
    ]
  
    
----------------
viewSand :: View Model Action
viewSand = 
  H.img_
    [ className "sand-background"
    , HP.src_ "/static/sand.png"
    , HP.alt_ "Sand background"
    ]

viewWater :: View Model Action
viewWater = 
  H.img_
    [ className "water-background"
    , HP.src_ "/static/water.png"
    , HP.alt_ "Water background"  
    ]

----------------------------------------------------------------------------


{-  H.svg_
        [ SP.viewBox_ "0 0 600 600"
        , CSS.style_
          [ CSS.width "600px"
          , CSS.height "600px"
          , CSS.borderStyle "solid"
          ]
        ] -}




{- pointsText :: [(Double, Double)] -> MisoString -- 
pointsText points =
  ms $ intercalate " "
    [ show x <> "," <> show y
    | (x, y) <- points
    ] -}
