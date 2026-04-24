module Catan where

import Types
import System.Random
import Data.Maybe (mapMaybe, isJust)

{-- 
- Init Game state
    1. Initialize empty board 
    2. Initialize Player state (Color, 5 settelments, 4 cities, 15 roads) 
    3. (Later) Players places 1 settelment and 1 road, repeat 2 for each player 

    4. Random player selected to start 
- Turn Loop 
    5. 2 Dice role - two paths
    6a. Resource distribution    |    6b. (Later) Robber placement and steal random card 
    7. (Later) Trading phase
    8. Building phase  (Road, Settelment, City, (Later) Dev card )
    9.  (Later) Play dev card 
    10. (Later) Update special cards (Longest road, Largest Army)
    11. Check VP > 10 -> Endgame 
    12. Next turn, repeat from 5 


-- MVP 

- Init Game state
    1. Initialize empty board 
    2. Initialize Player state (Color, 5 settelments, 4 cities, 15 roads) 
    3. Auto place first buildings and roads
    4. Random player selected to start 

- Turn Loop 
    5. 2 Dice role - two paths
    6. Resource distribution
    7. Building phase  (Road, Settelment, City)
    8. Check VP >= 10 -> Endgame 
    9. Next player, repeat from 5 
--}

diceResult :: IO (Int, Int)
diceResult = do dice1 <- randomRIO (1, 6)
                dice2 <- randomRIO (1, 6)
                return (dice1, dice2)

getResource :: GraphTile -> Maybe Resource
getResource = resource . dataTile

-- this function is for a filter (GraphTile -> Bool) 
getResourcesFromTiles :: (GraphTile -> Bool) -> GraphNode -> [Resource]
getResourcesFromTiles f (GraphNode dat _ t) = case building dat of
    Just (Settlement _) -> r
    Just (City _      ) -> r ++ r
    Nothing             -> []
    where r = mapMaybe getResource (filter f t)

getResourcesOfNum :: Int -> Player -> [Resource]
getResourcesOfNum n p = concatMap (getResourcesFromTiles hasToken . snd) $ buildingFilter (buildings p)
    where
        hasToken gt = n == token (dataTile gt)
        
        buildingFilter = filter (hasBuilding . snd) -- It just has to be here since haskell has to 100% make sure the building exists
        hasBuilding (GraphNode dn _ _) = isJust $ building dn
