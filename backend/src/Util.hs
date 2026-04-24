module Util where 

import System.Random
import Data.UUID

someUUIDs :: [UUID]
someUUIDs =
  let seed = 123
      g0 = mkStdGen seed -- RNG from seed
      (u1, g1) = random g0
      (u2, g2) = random g1
      (u3, g3) = random g2
      (u4, _) = random g3
  in [u1,u2,u3,u4]

oneUUID :: UUID
oneUUID = fst $ random g0 
 where 
    g0 = mkStdGen 125