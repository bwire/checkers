module HumanPlayer (human) where

import Data.Char (toUpper)
import Control.Monad.Trans.Except

import Types
import Board (availableMoves, newBoard)

-- real player actions
human :: MoveType -> Board -> Side -> IO Board
human Move board side =
  case availableMoves board side of
    [] -> return board
    variants -> do 
      putStrLn $ show side ++ "s' turn. Enter you move."
      sm <- getLine
      putStrLn sm
      --v <- parseMoveString mdata variants
      --let b = move board side v
      
      -- -- debug information
      -- putStrLn $ "Step " ++ show (position b) ++
      --    ": (" ++ show side ++ " moved " ++ 
      --    formatCoords board (from v) ++ "-" ++ 
      --    formatCoords board (to v) ++ ")"
      
      return board

-- -- For the sake of simplicity initial assumption is for player to provide the position (something like "G2" or "B5")
-- -- Then we should seek in the list and if the necessary information gets found - return this move info
-- parseMoveString :: Board -> [MoveInfo] -> IO MoveInfo
-- parseMoveString board moves = do
--   mdata <- getLine
--   case parseCoords board mdata of
--     Just coords -> return moves !! 0 -- stub
--     Nothing -> do
--       putStrLn "Wrong value. Truy once more"
--       parseMoveString board moves
  
-- conert string representation into Coords
-- to be moved to Board!!!!!!
parseCoords :: Board -> String -> ExceptT String IO Coords
parseCoords board (x:xs) = 
  return (x, xs) >>= getXCoord board >>= checkYCoord board
parseCoords _ [] = throwE "Wrong cell format"

-- determine X coordinate on the board by the given letter
-- TODO: Put inside human. Git rid off board!!
getXCoord :: Board -> (Char, String) -> ExceptT String IO (Int, String)
getXCoord board (xc, yc) =
  let ur = toUpper xc
      xletters = take (width board) ['A'..]
  in case elem ur xletters of
    True -> return . (\x -> (x, yc)) . snd . last $ zip ['A'..ur] [1..]
    False -> throwE "Wrong X value. Truy once more"

-- check Y value to be withing a range
checkYCoord :: Board -> (Int, String) -> ExceptT String IO Coords
checkYCoord board (x, yc) = 
  let y = read yc
  in case y > 0 && y <= (height board) of
      True -> return (x, y)
      False -> throwE "Wrong Y value. Truy once more"


 


-- -- only if the attack ia available
-- human Attack board side = do
--   v <- getRandom (availableAttacks board side)
--   let b = attack board side v
  
--   -- debug information
--   putStrLn $ "Step " ++ show (position b) ++
--      ": (" ++ show side ++ " attacked " ++ 
--      formatCoords board (from v) ++ "-" ++ 
--      formatCoords board (to v) ++ ", checker at " ++ 
--      formatCoords board (victim v) ++ " removed)" 
  
--   return b