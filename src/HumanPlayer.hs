module HumanPlayer (human) where

import Data.Char (isAlphaNum, toUpper)
import Data.List (find)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)

import Types
import Board (availableMoves, formatCoords, move, newBoard)

-- real player actions
human :: MoveType -> GameInfo -> Side -> IO GameInfo
human Move info side = 
  case availableMoves (board info) side of
    [] -> return info
    variants -> do 
      putStrLn $ show side ++ "s' turn. Enter you move:"
      res <- runExceptT $ parseMove info variants
      case res of
        -- in this case Left part should contain just Quit - all other situations related to the wrong input values and so on shoulbe treated inside inner functions
        Left _ -> return Quit
        Right v -> do
          let b = move (board info) side v      
          putStrLn $ "Step " ++ show (position b) ++
            ": (" ++ show side ++ " moved " ++ 
            formatCoords b (from v) ++ "-" ++ 
            formatCoords b (to v) ++ ")"
          return $ info { board = b } 
  where
    -- For the sake of simplicity initial assumption is for player to provide the position (something like "G2" or "B5")
    -- Then we should seek in the list and if the necessary information gets found - return this move info
    -- TODO Check the valididy of the approach - using GameInfo in the Left part
    -- TODO git rid of case!!
    parseMove :: GameInfo -> [MoveInfo] -> ExceptT ParseFailure IO MoveInfo
    parseMove info moves = do
      smove <- liftIO getLine
      -- in case of wrong input just force to repeat (until Quit is demanded)
      catchE (checkQuit smove >>= parseInput >>= checkCoords) handler 
      where
        brd = board info
        handler e = case e of
          (WrongMove s) -> do
            liftIO $ putStrLn s
            parseMove info moves
      
        -- this is only the place where user input is checked for quit
        checkQuit :: String -> ExceptT ParseFailure IO String
        checkQuit s | map toUpper s == "Q" = throwE ForceQuit
                    | otherwise = return s

        -- check if parsed coords are withing available list of coords
        checkCoords :: (Coords, Coords) -> ExceptT ParseFailure IO MoveInfo                
        checkCoords (fromCoords, toCoords) = case find (\m -> from m == fromCoords && to m == toCoords) moves of
          Just i -> return i
          --Nothing -> throwE $ WrongMove $ "This move isn't available! " ++ (map show moves)
          -- for debug
          Nothing -> do 
            liftIO . putStrLn $ (show fromCoords ++ "-" ++ show toCoords)
            mapM_ (liftIO . putStrLn . show) moves
            throwE $ WrongMove "This move isn't available! "
        
        -- the coordinates should be in form A5:C3 - (initial point:destination point)
        parseInput :: String -> ExceptT ParseFailure IO (Coords, Coords)
        parseInput input = do
          let (from, rest) = span (\c -> c /= ' ' && c /= ':') input
          fromCoords <- parseCoords from
          toCoords <- parseCoords $ dropWhile (not . isAlphaNum) rest
          return (fromCoords, toCoords)
        
        parseCoords :: String -> ExceptT ParseFailure IO Coords
        parseCoords (c:cs) = do
          y <- getYCoord c
          x <- getXCoord cs
          return (x, y)
          where
            -- check Y value to be withing a range
            getYCoord :: Char -> ExceptT ParseFailure IO Int
            getYCoord c =
              let ur = toUpper c
                  xletters = take (width brd) ['A'..]
              in case elem ur xletters of
                True -> return . snd . last $ zip ['A'..ur] [1..]
                False -> throwE $ WrongMove "Wrong X value. Truy once more"
            -- convert string representation into Coords     
            getXCoord :: String -> ExceptT ParseFailure IO Int
            getXCoord yc = 
              let y = read yc
                  hb = height brd
              in case y > 0 && y <= hb of
                True -> return $ hb + 1 - y 
                False -> throwE $ WrongMove "Wrong Y value. Truy once more"
        parseCoords _ = throwE $ WrongMove "Wrong input format"
        


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