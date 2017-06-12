module Main where

import Types
import Checkers (play, selectBoard, selectPlayers)
import RandomChoices (randomComputer)

import Control.Monad.Trans.Except (runExceptT)

main :: IO String 
main = do
  result <- runExceptT $ selectBoard >>= selectPlayers
  case result of 
    Right info -> play (board info) randomComputer randomComputer 
    Left error -> return error
