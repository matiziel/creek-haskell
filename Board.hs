module Board
  ( Board (..),
    BoardCell (..),
    BoardSize,
    generateBoard,
  )
where

type BoardSize = (Int, Int)

data BoardCell = Empty | Filled | Unknown deriving (Eq, Show)

data Board = Board BoardSize [[BoardCell]] deriving (Eq, Show)

generateBoard :: BoardSize -> Board
generateBoard (height, width) = Board (height, width) cells
  where
    cells = [[Unknown | x <- [1 .. width]] | x <- [1 .. height]]