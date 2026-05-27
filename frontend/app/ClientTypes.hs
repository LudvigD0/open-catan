module ClientTypes where

import Miso (MisoString)
import Types


-- | Vår model som miso såklart behöver för att ändra tillstånd
data Model = Model
  { gameState :: Maybe GameState
  , selectedNode :: Maybe NodeId
  , selectedEdge :: Maybe EdgeId
  , requestStatus :: RequestStatus
  } deriving Eq


-- | Status för requesten
data RequestStatus
  = Idle
  | Loading MisoString
  | RequestFailed MisoString
  | ServerRejected GameError
  deriving Eq


-- | Våra miso-Actions
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
