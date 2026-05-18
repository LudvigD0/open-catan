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
import Miso.Html.Event (onClick)
import Miso.Html.Property (className, id_)

import Api
import BoardView
import Types
import ClientTypes



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

updateModel ClickStart = do
  appModel <- get
  put appModel { requestStatus = Loading "Starting game..." }
  requestStartGame

updateModel (ClickNode nid) = do
  appModel <- get
  put appModel
    { selectedNode = Just nid
    , requestStatus = Loading "Sending settlement action..."
    }
  requestGameAction (ActBuildSettlement nid)

updateModel (ClickEdge eid) = do
  appModel <- get
  put appModel
    { selectedEdge = Just eid
    , requestStatus = Loading "Sending road action..."
    }
  requestGameAction (ActBuildRoad eid)

updateModel ClickRollDice = do
  appModel <- get
  put appModel { requestStatus = Loading "Rolling dice..." }
  requestGameAction ActRollDice

updateModel ClickEndTurn = do
  appModel <- get
  put appModel { requestStatus = Loading "Ending turn..." }
  requestGameAction ActEndTurn

updateModel (GotGameResponse response) = do
  appModel <- get
  case response of
    GameStateResponse gs ->
      put appModel
        { gameState = Just gs
        , requestStatus = Idle
        }
    GameErrorResponse err ->
      put appModel { requestStatus = ServerRejected err }
      
updateModel (ApiRequestFailed message) = do
  appModel <- get
  put appModel { requestStatus = RequestFailed message }

initialModel :: Model
initialModel = Model
  { gameState = Nothing
  , selectedNode = Nothing
  , selectedEdge = Nothing
  , requestStatus = Idle
  }

--- model fungerar som wrappern för allt den ritar ut (body ungefär)
viewModel :: Model -> View Model Action
viewModel appModel =
  H.div_
    [ id_ "container", className "container" ]
    [ viewWater
    , viewSand
    , viewGame appModel
    ]

viewGame :: Model -> View Model Action
viewGame appModel =
  case gameState appModel of
    Nothing -> viewStartMenu appModel
    Just gs ->
      H.div_
        []
        [ viewTopMenu appModel gs
        , viewBoard gs
        ]

viewStartMenu :: Model -> View Model Action
viewStartMenu appModel =
  H.div_
    [ className "start-menu" ]
    [ H.h1_ [] [ text "Open Catan" ]
    , H.button_
        [ className "start-button"
        , onClick ClickStart
        ]
        [ text "Start" ]
    , viewRequestStatus appModel
    ]

viewTopMenu :: Model -> GameState -> View Model Action
viewTopMenu appModel gs =
  H.div_
    [ className "top-menu" ]
    (rollButton ++
    [ H.button_
        [ className "menu-button"
        , onClick ClickEndTurn
        ]
        [ text "End turn" ]
    , H.span_
        [ className "turn-status" ]
        [ text ("Turn: " <> ms (show (currentTurn gs)) <> " / " <> ms (show (turnPhase gs))) ]
    , viewDice gs
    , viewRequestStatus appModel
    ])
  where
    rollButton =
      case turnPhase gs of
        Roll ->
          [ H.button_
              [ className "menu-button"
              , onClick ClickRollDice
              ]
              [ text "Roll" ]
          ]
        _ -> []

viewDice :: GameState -> View Model Action
viewDice gs =
  case dice gs of
    (0, 0) -> H.span_ [ className "dice-status" ] []
    (d1, d2) ->
      H.span_
        [ className "dice-status" ]
        [ text ("Dice: " <> ms (show d1) <> " + " <> ms (show d2) <> " = " <> ms (show (d1 + d2))) ]

viewRequestStatus :: Model -> View Model Action
viewRequestStatus appModel =
  case requestStatus appModel of
    Idle -> H.span_ [ className "request-status" ] []
    Loading message ->
      H.span_ [ className "request-status" ] [ text message ]
    RequestFailed message ->
      H.span_ [ className "request-status request-status-error" ] [ text message ]
    ServerRejected err ->
      H.span_ [ className "request-status request-status-error" ] [ text (ms (show err)) ]
  
    
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
