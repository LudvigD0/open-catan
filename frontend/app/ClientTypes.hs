module ClientTypes where

import Miso (MisoString)
import Types

data Model = Model
  { gameState :: Maybe GameState
  , selectedNode :: Maybe NodeId
  , selectedEdge :: Maybe EdgeId
  , requestStatus :: RequestStatus
  } deriving Eq

data RequestStatus
  = Idle
  | Loading MisoString
  | RequestFailed MisoString
  | ServerRejected GameError
  deriving Eq

data Action
  = NoOp
  | ClickStart
  | ClickHex
  | ClickNode NodeId
  | ClickEdge EdgeId
  | ClickRollDice
  | ClickEndTurn
  | GotGameResponse GameResponse
  | ApiRequestFailed MisoString
  deriving Eq
