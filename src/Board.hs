module Board (
  attack,
  availableAttacks,
  availableMoves,
  checkers8x8, 
  checkers10x10, 
  displayBoard, 
  formatCoords,
  move,
  newBoard, 
  otherSide, 
  pieces, 
  upgradeToKings
) where

import Types

-- gather possible attacks information
availableAttacks :: Board -> Side -> [MoveInfo]
availableAttacks board side = collectOpportunities board side canTake genAttackInfo
  where
  
    canTake :: Piece -> [Coords] -> Bool
    --checker can take only if there is an enemy checker in front of it and an empty cell behind
    canTake Checker (inFront : behindIt : _) = 
      hasPiece board (otherSide side) inFront && empty board behindIt
    canTake King diagonal = canTake Checker $ nearestPiece
      where nearestPiece = dropWhile (empty board) diagonal
    canTake _ _ = False
    
    genAttackInfo :: Piece -> Coords -> [Coords] -> [MoveInfo]
    -- a checker takes jumping over the nearest daigonal cell to the cell next to it
    genAttackInfo Checker coords diagonal = [ AttackInfo { piece = Checker, from = coords, victim = diagonal !! 0, to = diagonal !! 1 } ]
    genAttackInfo King coords diagonal = map leapOverNearestPieces landingPlaces
      where 
        leapOverNearestPieces square = AttackInfo { piece = King, from = coords, victim = nearestPiece, to = square }
        (nearestPiece : behindNearestPiece) = dropWhile (empty board) diagonal
        landingPlaces = takeWhile (empty board) behindNearestPiece
    
-- gather informatiom about available moves
-- 'canMove' realizes select cryteria and 'genMoveInfo' generates necessary information about every possible move
availableMoves :: Board -> Side -> [MoveInfo]
availableMoves board side = collectOpportunities board side canMove genMoveInfo
  where
    -- Checker can move only if there is an empty cell in fron of it
    canMove :: Piece -> [Coords] -> Bool
    canMove Checker (inFront:_) = empty board inFront 
    -- King can move if it has one or several empty cells in front of it
    canMove King diagonal = not $ null $ squaresToObstacles diagonal
    canMove _ _ = False
    
    genMoveInfo :: Piece -> Coords -> [Coords] -> [MoveInfo] 
    genMoveInfo Checker coords diagonal = [MoveInfo { piece = Checker, from = coords, to = head diagonal }]
    genMoveInfo King coords diagonal = map moveTo (squaresToObstacles diagonal) where
      moveTo square = MoveInfo { piece = King, from = coords, to = square }
      
    squaresToObstacles :: [Coords] -> [Coords]
    squaresToObstacles = takeWhile (empty board) 
       
-- Board modification while beating enemy checker. piece takes enemy part 'from' cell 'to' cell, jumping over 'victim' coords.
-- So we move 'peice' 'from' position 'to' position, amd remove victim from the board
-- При этом мы перемещаем фигуру `piece' указанного цвета на новое
-- место, а затем убираем с доски фигуру-жертву противоположного
-- цвета.
attack :: Board -> Side -> MoveInfo -> Board
attack board side (AttackInfo piece from victim to) =
  remove victim (otherSide side) $ 
    replace (from, piece) (to, piece) side board

checkers8x8 = newBoard 8 8 12
checkers10x10 = newBoard 10 10 20

-- gathering information about certain piece
collectOpportunities :: Board 
                     -> Side 
                     -> (Piece -> [Coords] -> Bool) 
                     -> (Piece -> Coords -> [Coords] -> [MoveInfo])
                     -> [MoveInfo]
collectOpportunities board side canAct describeAction = concatMap checkDiagonals (pieces side board) where
    -- the whole information about a piece is a summary of its possible directions (depending on the piece type)
    checkDiagonals :: (Coords, Piece) -> [MoveInfo] 
    checkDiagonals (coords, piece) = concatMap (checkDirection coords piece) (validDirections piece)

    -- if 'piece' located on the cell 'coords' and aimed to the 'cells' in the 'direction' satisfies the condition 'acnAct',
    -- then we gathe rinformation about this piece via the function 'describeAction'. Otherwise empty list returns
    checkDirection :: Coords -> Piece -> (Int, Int) -> [MoveInfo]
    checkDirection coords piece direction
      | canAct piece squares = describeAction piece coords squares
      | otherwise            = []
      where squares = diagonal coords direction
    
    -- Diagonal begins at the cell (row, column) and passes in the direction defied in (dr, dc)
    -- until the end of board
    diagonal :: Coords -> (Int, Int) -> [Coords]
    diagonal (row, column) (dr, dc) = 
      [(r, c) | (r, c) <- zip rows columns, inside board r c] 
      where
        rows = take (height board) [row + dr, row + dr + dr..]
        columns = take (width board) [column + dc, column + dc + dc..]
        inside board r c = r > 1 && c > 1 && r <= height board && c <= width board
    
    -- valid directions for a checker are onle forward, and for a king - all for directions
    validDirections :: Piece -> [(Int, Int)]
    validDirections Checker = [(forward, left), (forward, right)]
    validDirections King = [(forward, left), (forward, right), (backward, left), (backward, right)]
    (forward, backward, left, right) = 
      if side == White
      then (-1, 1, -1, 1)
      else (1, -1, 1, -1)

displayBoard :: Board -> IO ()
displayBoard board = putStrLn . boardToString $ board 

  where
    
    boardToString :: Board -> String
    boardToString board = unlines $ (withRowNumbers rows) ++ (lowerRow board : [])
    
    lowerRow :: Board -> String
    lowerRow board = "   " ++ (map fst (zip ['A' ..] [1..width board]))
    
    withRowNumbers :: [String] -> [String]
    withRowNumbers = zipWith (\n r -> (if n < 10 then " " else "") ++ show n ++ " " ++ r) [height board, height board - 1..] 
    
    rows :: [String]
    rows = [row r | r <- [1 .. height board]]

    row :: Int -> String
    row r = [square r c | c <- [1 .. width board]]

    square :: Int -> Int -> Char
    square r c = case lookup (r, c) allpices of
      Just (Checker, White) -> 'w'
      Just (King, White) -> 'W'
      Just (Checker, Black) -> 'b'
      Just (King, Black) -> 'B'
      Nothing -> if isBlackSquare r c then '.' else ' '

    allpices :: [(Coords, (Piece, Side))]
    allpices = [(coords, (piece, side)) | side <- [Black, White], (coords, piece) <- pieces side board]

-- a cell considered empty if no pieces of either side located on it
empty :: Board -> Coords -> Bool
empty board coords = not $ hasPiece board White coords || hasPiece board Black coords 

-- conert Coords into string representation
formatCoords :: Board -> Coords -> String
formatCoords board (x, y) = 
  (\c -> c : show (height board + 1 - x)) 
    . snd 
    . head 
    . reverse $ zip [1..y] ['A'..]

-- helper to extract Coords from pair
getCoords :: PieceInfo -> Coords
getCoords (coords, _) = coords

-- self-explaining :)
isBlackSquare :: Int -> Int -> Bool
isBlackSquare r c = odd $ r + c

-- if selected player side contains a checker with 'coords' then True, otherwise False
hasPiece :: Board -> Side -> Coords -> Bool
hasPiece board side coords = 
  case lookup coords (pieces side board) of
    Nothing -> False
    _ -> True

-- Board modification: move piece 'from' cell 'to' cell
move :: Board -> Side -> MoveInfo -> Board
move board side (MoveInfo piece from to) =
  replace (from, piece) (to, piece) side board

-- generate initial board layout
newBoard :: Int -> Int -> Int -> Board
newBoard h w numPices = Board h w whitePieces blackPieces 0
  where
    coords :: [Coords]
    coords = [(row, column) | row <- [1..h], column <- [1..w], isBlackSquare row column]
  
    blackPieces :: Part
    blackPieces = zip (take numPices coords) (repeat Checker)

    whitePieces :: Part
    whitePieces = zip (take numPices (reverse coords)) (repeat Checker)
    
otherSide :: Side -> Side
otherSide White = Black
otherSide Black = White
    
-- helper to extract Side information list from the board
pieces :: Side -> (Board -> Part)
pieces White = whites
pieces Black = blacks

-- remove victim checker from the board.
remove :: Coords -> Side -> Board -> Board
remove victim side board = 
  let pcs = pieces side board
      pcs' = filter ((/= victim) . getCoords) pcs
  in setPieces board side pcs' False

-- move checher form one position to another
replace :: PieceInfo -> PieceInfo -> Side -> Board -> Board
replace (from, piece) (to, piece') side board = 
  let pcs = pieces side board
      pcs' = (to, piece') : (filter (/= (from, piece)) pcs)
  in setPieces board side pcs' True

-- update part
setPieces :: Board -> Side -> Part -> Bool -> Board 
setPieces board White ws inc = 
  let pos = if inc then position board + 1 else position board
  in board { whites = ws, position = pos }
setPieces board Black ws inc = 
  let pos = if inc then position board + 1 else position board
  in board { blacks = ws, position = pos }

-- checker turns into king. For each side check if there are any checkers at the far row - they become kings   
upgradeToKings :: Board -> Side -> Board  
upgradeToKings board side = newBoard
  where 
    kingRow :: Side -> Int
    kingRow White = 1 
    kingRow Black = height board

    newBoard :: Board
    newBoard = 
      let pcs = pieces side board
      in case filter (upgradableChecker (kingRow side)) pcs of
        (coords, checker) : _ -> replace (coords, checker) (coords, King) side board
        _ -> board

    upgradableChecker :: Int -> PieceInfo -> Bool    
    upgradableChecker targetRow ((row, _), _) = targetRow == row
