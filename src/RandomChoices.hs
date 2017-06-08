module RandomChoices (randomComputer) where

import Types
import Board (attack, availableMoves, availableAttacks, formatCoords, move)
import System.Random


getRandom :: [MoveInfo] -> IO MoveInfo
getRandom lst = do
  idx <- randomRIO (0, length lst - 1)
  return $ lst !! idx

--  Dummy computer bot. One of the available moves or attacks is selected.
randomComputer :: MoveType -> Board -> Side -> IO Board
randomComputer Move board side =
  case availableMoves board side of
    [] -> return board
    variants -> do 
      --TODO DELETE putStrLn $ show (length variants)
      v <- getRandom variants
      let b = move board side v
      
      -- debug information
      putStrLn $ "Position " ++ show (position b) ++
         ": (" ++ show side ++ " moved " ++ 
         formatCoords board (from v) ++ "-" ++ 
         formatCoords board (to v) ++ ")"
      
      return b
      
-- only if the attack ia available
randomComputer Attack board side = do
  v <- getRandom (availableAttacks board side)
  let b = attack board side v
  
  -- debug information
  putStrLn $ "Position " ++ show (position b) ++ ":" 
  
  return b
 