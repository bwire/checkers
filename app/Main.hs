module Main where

import Types
import Board (checkers8x8, checkers10x10)
import Checkers (play)
import RandomChoices (randomComputer)

getConfig :: IO (Board, Player, Player)
getConfig = return (checkers8x8, randomComputer, randomComputer)

main :: IO String 
main = do
  (newBoard, playerA, playerB) <- getConfig
  play newBoard playerA playerB