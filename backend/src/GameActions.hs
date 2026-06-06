-- | Effectful action interpreter for API requests.
module GameActions
    ( applyGameAction
    , startGame
    ) where

import System.Random (randomRIO)

import Catan
  ( autoPlace
  , buildCity
  , buildRoad
  , buildSettlement
  , distributeResources
  , endTurn
  , initGameState
  )
import PureUtil (someUUIDs)
import Types

-- | Initial game state used when the API starts a new local game.
startGame :: GameState
startGame = autoPlace (initGameState someUUIDs 0)

-- | Apply one client action to a game state.
--
-- The interpreter enforces the current turn phase before delegating placement
-- and resource checks to the pure common library.
applyGameAction :: GameAction -> GameState -> IO (Either GameError GameState)
applyGameAction ga gs =
    case ga of
        ActRollDice
            | turnPhase gs == Roll -> do
                diceRoll <- rollDice
                case applyRollDice diceRoll gs of
                    Nothing -> return $ Left InvalidGameAction
                    Just gs' -> return $ Right gs'
            | otherwise ->
                return $ Left (InvalidPhase (turnPhase gs) ga)
        ActBuildRoad eid
            | turnPhase gs == Build ->
                case buildRoad eid gs of
                    Nothing -> return $ Left InvalidRoadPlacement
                    Just gs' -> return $ Right gs'
            | otherwise ->
                return $ Left (InvalidPhase (turnPhase gs) ga)
        ActBuildSettlement nid
            | turnPhase gs == Build ->
                case buildSettlement nid gs of
                    Nothing -> return $ Left InvalidSettlementPlacement
                    Just gs' -> return $ Right gs'
            | otherwise ->
                return $ Left (InvalidPhase (turnPhase gs) ga)
        ActBuildCity nid
            | turnPhase gs == Build ->
                case buildCity nid gs of
                    Nothing -> return $ Left InvalidCityPlacement
                    Just gs' -> return $ Right gs'
            | otherwise ->
                return $ Left (InvalidPhase (turnPhase gs) ga)
        ActEndTurn
            | turnPhase gs == Build ->
                case endTurn gs of
                    Nothing -> return $ Left InvalidGameAction
                    Just gs' -> return $ Right gs'
            | otherwise ->
                return $ Left (InvalidPhase (turnPhase gs) ga)

-- | Apply an already-rolled dice result and enter the build phase.
applyRollDice :: (Int, Int) -> GameState -> Maybe GameState
applyRollDice roll@(r1, r2) gs =
    let newPlayers = distributeResources (r1 + r2) (board gs) (players gs)
    in Just gs
        { dice = roll
        , players = newPlayers
        , turnPhase = Build
        }

-- | Roll two six-sided dice.
rollDice :: IO (Int, Int)
rollDice = do
    d1 <- randomRIO (1, 6)
    d2 <- randomRIO (1, 6)
    return (d1, d2)
