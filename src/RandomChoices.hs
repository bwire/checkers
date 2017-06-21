module RandomChoices (randomComputer) where

import Types
import Board (attack, availableMoves, availableAttacks, formatCoords, move)
import System.Random


getRandom :: [MoveInfo] -> IO MoveInfo
getRandom lst = do
  idx <- randomRIO (0, length lst - 1)
  return $ lst !! idx

--  Dummy computer bot. One of the available moves or attacks is selected.
randomComputer :: MoveType -> GameInfo -> Side -> IO GameInfo
randomComputer Move info side =
  case availableMoves (board info) side of
    [] -> return info
    variants -> do 
      v <- getRandom variants
      let b = move (board info) side v
      
      putStrLn $ "Step " ++ show (position b) ++
         ": (" ++ show side ++ " moved " ++ 
         formatCoords b (from v) ++ "-" ++ 
         formatCoords b (to v) ++ ")" 
      return $ info { board = b }
      
-- only if the attack ia available
randomComputer Attack info side = do
  v <- getRandom (availableAttacks (board info) side)
  let b = attack (board info) side v
  
  putStrLn $ "Step " ++ show (position b) ++
     ": (" ++ show side ++ " attacked " ++ 
     formatCoords b (from v) ++ "-" ++ 
     formatCoords b (to v) ++ ", checker at " ++ 
     formatCoords b (victim v) ++ " removed)" 
  return $ info { board = b }
 