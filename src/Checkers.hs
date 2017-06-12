module Checkers (play, selectBoard, selectPlayers) where

import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import System.Exit

import Types
import Board (availableAttacks, checkers8x8, checkers10x10, displayBoard, newBoard, otherSide, pieces, upgradeToKings)


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
    
    -- If player can attack more than one piece he has to do it.
    -- It's not necessary to make move with the maximum attacking moves
    attackLoop :: Player -> Board -> Side -> IO Board
    attackLoop player board side = do
      board' <- player Attack board side
      
      if canAttack board' side
      then attackLoop player board' side 
      else return board'
    
    getPlayer :: Side -> Player  
    getPlayer White = playerA
    getPlayer Black = playerB
    
    canAttack :: Board -> Side -> Bool
    canAttack board side = not $ null $ availableAttacks board side  
    
    isVictory :: Side -> Board -> Bool
    isVictory s b = null (pieces (otherSide s) b) || position b >= 20-- board selection in the beginning


-- board selection in the beginning
selectBoard :: ExceptT String IO GameInfo
selectBoard = do
  liftIO $ 
    putStrLn "Select board type:" >>
    putStrLn "  8 - 8x8 board" >>
    putStrLn "  10 - 10x10 board" >>
    putStrLn "  q - quit"
  val <- liftIO getLine
  case val of
    "8" -> return $ GameInfo checkers8x8
    "10" -> return $ GameInfo checkers10x10
    "q" -> throwE "Quit game"
    _ -> throwE "Incorrect board selection"

selectPlayers :: GameInfo -> ExceptT String IO GameInfo
selectPlayers info = return info





