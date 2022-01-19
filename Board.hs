module Board
  ( Board (..),
    BoardCell (..),
    BoardSize,
    BoardCellIndex,
    generateBoard,
    boardCellValueAt,
    setBoardCells,
    replaceUknownCells,
  )
where

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
setBoardCells = foldl setBoardCell

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

-- getCellsWithValue :: Board -> BoardCell -> [BoardCellIndex]
-- getCellsWithValue (Board (height, width) cells) value = [(1, 1)]

-- validateBoard :: Board -> Bool
-- validateBoard b@(Board (height, width) cells)