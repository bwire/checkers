module Types where

data Side = White | Black deriving (Show, Eq)

data MoveType = Move | Attack

-- checker desctiption
data Piece = Checker | King deriving (Eq, Show)

-- board cell definition (coordinates)
type Coords = (Int, Int)

-- full piece description
type PieceInfo = (Coords, Piece)

-- just for simplicity
type Part = [PieceInfo]

-- board layout: sizes and to list of pairs (coordinates and checker type)
data Board = Board { 
  height :: Int, 
  width  :: Int, 
  whites :: Part, 
  blacks :: Part,
  position :: Int
}

data MoveInfo = 
  MoveInfo { piece :: Piece, from :: Coords, to :: Coords} | 
  AttackInfo { piece :: Piece, from :: Coords, victim :: Coords, to :: Coords } deriving Show 

type Player = MoveType -> Board -> Side -> IO Board
