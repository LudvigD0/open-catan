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
diceResult = do dice1 <- randomR (1, 6)
                dice2 <- randomR (1, 6)
                return (dice1, dice2)

getResourcesOfNum :: Board -> Player -> Int -> [Resource]
getResourcesOfNum brd n p = undefined



