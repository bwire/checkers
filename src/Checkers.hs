module Checkers (play) where

import Types
import Board (availableAttacks, displayBoard, newBoard, otherSide, pieces, upgradeToKings)

play :: Board -> Player -> Player -> IO String
play newBoard playerA playerB = do
  
  putStrLn ""
  
   -- debug information
  putStrLn $ "Initial position:" 
  displayBoard newBoard
  
  -- whires make first move
  makeMove newBoard White where
    makeMove :: Board -> Side -> IO String
    makeMove board side = do
      
      -- A player playing for 'side' has to attack if he has possibility.
      -- Otherwise he can make any move possible.
      let player = getPlayer side
      afterMove <- if canAttack board side 
                   then attackLoop player board side
                   else player Move board side
      
      -- checkers turn into kings agter the move (not while moving)
      let nextBoard = upgradeToKings afterMove side
      
      displayBoard nextBoard
      if isVictory side nextBoard
      then return $ "Winner is " ++ show side
      else makeMove nextBoard (otherSide side)
    
    -- Если игрок может "взять" больше одной фигуры противника - он
    -- должен это сделать. Мы не требуем ходить фигурой,
    -- способной выполнить максимальное количество взятий.
    attackLoop player board side = do
      board' <- player Attack board side
      
      if canAttack board' side
      then attackLoop player board' side 
      else return board'
      
    getPlayer White = playerA
    getPlayer Black = playerB
    
    canAttack :: Board -> Side -> Bool
    canAttack board side = not $ null $ availableAttacks board side  
    
    isVictory :: Side -> Board -> Bool
    isVictory s b = null (pieces (otherSide s) b) || position b >= 20