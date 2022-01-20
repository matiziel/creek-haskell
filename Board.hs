module Board
  ( Board (..),
    BoardCell (..),
    BoardSize,
    BoardCellIndex,
    generateBoard,
    boardCellValueAt,
    setBoardCells,
    replaceUknownCells,
    validateCreek,
  )
where

import Data.Set (Set, empty, insert, notMember, size)

type BoardSize = (Int, Int)

type BoardCellIndex = (Int, Int)

data BoardCell = Empty | Filled | Unknown deriving (Eq, Show)

data Board = Board BoardSize [[BoardCell]] deriving (Eq, Show)

generateBoard :: BoardSize -> Board
generateBoard (height, width) = Board (height, width) cells
  where
    cells = [[Unknown | x <- [1 .. width]] | x <- [1 .. height]]

boardCellValueAt :: Board -> BoardCellIndex -> BoardCell
boardCellValueAt (Board _ cells) (xarg, yarg) = (cells !! xarg) !! yarg

setBoardCell :: Board -> (BoardCellIndex, BoardCell) -> Board
setBoardCell b@(Board (height, width) cells) ((xarg, yarg), value) =
  Board
    (height, width)
    [[(if x == xarg && y == yarg then value else boardCellValueAt b (x, y)) | y <- [0 .. width - 1]] | x <- [0 .. height - 1]]

setBoardCells :: Board -> [(BoardCellIndex, BoardCell)] -> Board
setBoardCells b [] = b
setBoardCells b (x : xs) = setBoardCells (setBoardCell b x) xs

replaceUknownCells :: Board -> Board
replaceUknownCells b@(Board (height, width) cells) =
  Board
    (height, width)
    [ [ ( if boardCellValueAt b (x, y) == Unknown
            then Empty
            else boardCellValueAt b (x, y)
        )
        | y <- [0 .. width - 1]
      ]
      | x <- [0 .. height - 1]
    ]

validateCreek :: Board -> Bool
validateCreek b@(Board (height, width) _) = size creek == length emptyOrUnknownCells
  where
    emptyOrUnknownCells = getEmptyOrUnknownCells b
    creek = getCreekForValidation b [head emptyOrUnknownCells] empty

getEmptyOrUnknownCells :: Board -> [BoardCellIndex]
getEmptyOrUnknownCells b@(Board (height, width) _) =
  filter (\(x, y) -> boardCellValueAt b (x, y) == Empty || boardCellValueAt b (x, y) == Unknown) cells
  where
    cells = generateBoardCells (height, width)

generateBoardCells :: BoardSize -> [BoardCellIndex]
generateBoardCells (height, width) = [(x, y) | x <- [0 .. height - 1], y <- [0 .. width - 1]]

getCreekForValidation :: Board -> [BoardCellIndex] -> Set BoardCellIndex -> Set BoardCellIndex
getCreekForValidation b [] set = set
getCreekForValidation b (x : xs) set =
  getCreekForValidation b (xs ++ neighboursNotVisited) newSet
  where
    newSet = insert x set
    emptyNeighbours = getEmptyOrUnknownNeighbours b x
    neighboursNotVisited = filter (\(x, y) -> notMember (x, y) set) emptyNeighbours

getEmptyOrUnknownNeighbours :: Board -> BoardCellIndex -> [BoardCellIndex]
getEmptyOrUnknownNeighbours b index =
  filter (\(x, y) -> boardCellValueAt b (x, y) == Empty || boardCellValueAt b (x, y) == Unknown) neighbours
  where
    neighbours = getNeighbours b index

getNeighbours :: Board -> BoardCellIndex -> [BoardCellIndex]
getNeighbours b@(Board (height, width) cells) (x, y)
  | x == 0 && y == 0 = [(x + 1, y), (x, y + 1)]
  | x == (height - 1) && y == (width - 1) = [(x - 1, y), (x, y - 1)]
  | x == 0 && y == (width - 1) = [(x + 1, y), (x, y - 1)]
  | (x == height - 1) && y == 0 = [(x - 1, y), (x, y + 1)]
  | y == 0 = [(x - 1, y), (x, y + 1), (x + 1, y)]
  | x == 0 = [(x, y - 1), (x + 1, y), (x, y + 1)]
  | y == (width - 1) = [(x - 1, y), (x, y - 1), (x + 1, y)]
  | x == (height - 1) = [(x, y - 1), (x - 1, y), (x, y + 1)]
  | otherwise = [(x, y - 1), (x - 1, y), (x, y + 1), (x + 1, y)]
