module Checkers (play, selectBoard, selectPlayers) where

import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import RandomChoices (randomComputer)
import System.Exit

import Types
import Board (availableAttacks, checkers8x8, checkers10x10, displayBoard, newBoard, otherSide, pieces, upgradeToKings)
import HumanPlayer (human)

-- game process main function
play :: GameInfo -> IO String
play info = do  
  putStrLn ""
  putStrLn $ "Initial position:" 
 
  displayBoard $ board info
  
  -- whites make first move
  makeMove info White where
  
    makeMove :: GameInfo -> Side -> IO String
    makeMove info side = do
      -- A player playing for 'side' has to attack if there is any possibility.
      -- Otherwise he can make any move possible.
      let player = getPlayer side
      newInfo <- if canAttack (board info) side 
                 then attackLoop player info side
                 else player Move info side
      
      case newInfo of
        Quit -> return $  show side ++ " quited the game. The winner is " ++ show (otherSide side)
        _ -> do
          let nextBoard = upgradeToKings (board newInfo) side -- checkers turn into kings after the move (not while moving)      
          displayBoard nextBoard
          if isVictory side nextBoard
          then return $ "Winner is " ++ show side
          else makeMove (info { board = nextBoard }) (otherSide side)
          
    -- If player can attack more than one piece he has to do it.
    -- It's not necessary to make move with the maximum attacking moves
    attackLoop :: Player -> GameInfo -> Side -> IO GameInfo
    attackLoop player info side = do
      info' <- player Attack info side
      if canAttack (board info') side
      then attackLoop player info' side 
      else return info'
    
    getPlayer :: Side -> Player  
    getPlayer White = whitePlayer info
    getPlayer Black = blackPlayer info
    
    canAttack :: Board -> Side -> Bool
    canAttack board side = not $ null $ availableAttacks board side  
    
    isVictory :: Side -> Board -> Bool
    isVictory s b = null (pieces (otherSide s) b) 
      || position b >= 20 -- TODO Remove after debug!!

-- board selection in the beginning
selectBoard :: ExceptT ParseFailure IO GameInfo
selectBoard = do
  liftIO $ 
    putStrLn "Select board type:" >>
    putStrLn "  8 - 8x8 board" >>
    putStrLn "  10 - 10x10 board" >>
    putStrLn "  q - quit"
  val <- liftIO getLine
  case val of
    "8" -> return $ GameInfo checkers8x8 randomComputer randomComputer
    "10" -> return $ GameInfo checkers10x10 randomComputer randomComputer
    "q" -> throwE $ ForceQuit
    _ -> do
      liftIO $ putStrLn "Wrong selection. Try once more.."
      selectBoard

selectPlayers :: GameInfo -> ExceptT ParseFailure IO GameInfo
selectPlayers info = 
  return info >>= selectPlayer White >>= selectPlayer Black    
      
selectPlayer :: Side -> GameInfo -> ExceptT ParseFailure IO GameInfo
selectPlayer side info = do
  liftIO $
    putStrLn ("Select " ++ show side ++ " player:") >>
    putStrLn "  c - computer" >>
    putStrLn "  h - human" >>
    putStrLn "  q - quit"
  val <- liftIO getLine
  case val of
    "c" -> return info -- no changes
    "h" -> case side of
      White -> return $ info { whitePlayer = human }
      Black -> return $ info { blackPlayer = human }
    "q" -> throwE ForceQuit
    _ -> do
      liftIO $ putStrLn "Wrong selection. Try once more.."
      selectPlayer side info