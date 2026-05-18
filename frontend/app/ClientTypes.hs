module ClientTypes where

import Types

data Model = Model
  { gameState :: GameState
  , selectedNode :: Maybe NodeId
  , selectedEdge :: Maybe EdgeId
  } deriving Eq


data Action
  = NoOp
  | ClickHex
  | ClickNode NodeId
  | ClickEdge EdgeId
  deriving (Show, Eq)
