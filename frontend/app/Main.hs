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

import qualified Data.Map as Map

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
-- | Updates model
updateModel :: Action -> Effect parent Model Action
updateModel NoOp = pure ()
updateModel ClickHex = pure ()

updateModel ClickStart = do
  appModel <- get
  put appModel { requestStatus = Loading "Starting game..." }
  requestStartGame

updateModel (ClickNode nid) = do
  appModel <- get
  let action =
        case gameState appModel of
          Just gs
            | nodeHasBuilding nid gs -> ActBuildCity nid
          _ -> ActBuildSettlement nid
      requestMessage =
        case action of
          ActBuildCity _ -> "Sending city action..."
          _              -> "Sending settlement action..."
  put appModel
    { selectedNode = Just nid
    , requestStatus = Loading requestMessage
    }
  requestGameAction action

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

nodeHasBuilding :: NodeId -> GameState -> Bool
nodeHasBuilding nid gs =
  case Map.lookup nid (nodes (board gs)) of
    Just boardNode -> case building boardNode of
      Just _  -> True
      Nothing -> False
    Nothing -> False

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
        , viewDice gs
        , viewResourcePanel gs
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
    (0, 0) -> H.div_ [ className "dice-display dice-display-empty" ] []
    (d1, d2) ->
      H.div_
        [ className "dice-display" ]
        [ viewDiceImage d1
        , viewDiceImage d2
        ]

viewDiceImage :: Int -> View Model Action
viewDiceImage value =
  H.img_
    [ className "dice-image"
    , HP.src_ (ms ("/static/dice/" ++ show value ++ ".png"))
    , HP.alt_ (ms ("Dice " ++ show value))
    ]

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

-- | Enkel div wrapper för att placera ut players resource-kort 
viewResourcePanel :: GameState -> View Model Action
viewResourcePanel gs =
  H.div_
    [ className "resource-panel" ]
    [ H.div_
        []
        (map (viewPlayerResources gs) (take 2 playerColors))

    , H.div_
        []
        (map (viewPlayerResources gs) (drop 2 playerColors))
    ]



viewPlayerResources :: GameState -> Color -> View Model Action
viewPlayerResources gs color =
  let playerResources =
        case Map.lookup color (players gs) of
          Just player -> resources player
          Nothing     -> Map.empty
  in
    H.div_
      [ className ("resource-player resource-player-" <> colorClass color) ]
      (map (viewResourceCard playerResources) resourceOrder)

viewResourceCard :: Map.Map Resource Int -> Resource -> View Model Action
viewResourceCard playerResources res =
  H.div_
    [ className "resource-card" ]
    [ H.img_
        [ HP.src_ (resourceCardImage res)
        , HP.alt_ (resourceName res)
        ]
    , H.span_
        [ className "resource-count" ]
        [ text (ms (show (Map.findWithDefault 0 res playerResources))) ]
    ]

playerColors :: [Color]
playerColors = [Red, Blue, Orange, White]

resourceOrder :: [Resource]
resourceOrder = [Lumber, Ore, Grain, Brick, Wool]

colorClass :: Color -> MisoString
colorClass color =
  case color of
    Red    -> "red"
    Blue   -> "blue"
    Orange -> "orange"
    White  -> "white"

resourceCardImage :: Resource -> MisoString
resourceCardImage res =
  case res of
    Lumber -> "/static/cards/card-wood.png"
    Ore    -> "/static/cards/card-ore.png"
    Grain  -> "/static/cards/card-wheat.png"
    Brick  -> "/static/cards/card-brick.png"
    Wool   -> "/static/cards/card-sheep.png"

resourceName :: Resource -> MisoString
resourceName res =
  case res of
    Lumber -> "Wood"
    Ore    -> "Ore"
    Grain  -> "Wheat"
    Brick  -> "Brick"
    Wool   -> "Sheep"
  
    
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
