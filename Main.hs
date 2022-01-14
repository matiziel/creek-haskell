import Board
import Data.List
import Rules
import System.IO

data Creek = Creek BoardSize Rules deriving (Read, Show)

main = do
  -- putStrLn "Podaj ścieżkę do pliku:"
  -- line <- getLine
  -- contents <- readFile line
  contents <- readFile "./test_files/test.txt"
  let creek = read contents :: Creek
  let board = generateBoard (getBoardSize creek)
  let rules = getRules creek
  print (solve board rules)

-- print board

getBoardSize :: Creek -> BoardSize
getBoardSize (Creek size _) = size

getRules :: Creek -> Rules
getRules (Creek _ rules) = rules

solve :: Board -> Rules -> Board
solve b@(Board (height, width) cells) rules = filledCells
  where
    rulesWithZero = filter (filterRules 0) rules
    emptyCells = fillCells b rulesWithZero Empty
    rulesWithFour = filter (filterRules 4) rules
    filledCells = fillCells emptyCells rulesWithFour Filled

filterRules :: Int -> Rule -> Bool
filterRules value ((a, b), c) = c == value

fillCells :: Board -> Rules -> BoardCell -> Board
fillCells board [] _ = board
fillCells b (x : xs) value =
  fillCells (applyRuleToBoard b x value) xs value

applyRuleToBoard :: Board -> Rule -> BoardCell -> Board
applyRuleToBoard b rule value = setBoardCells b [(index, value) | index <- getCellsForRuleApply b rule]

getCellsForRuleApply :: Board -> Rule -> [(Int, Int)]
getCellsForRuleApply (Board (height, width) cells) ((x, y), _)
  | x == 0 && y == 0 = [(0, 0)]
  | x == height && y == height = [(height - 1, width - 1)]
  | x == 0 && y == width = [(0, y - 1)]
  | x == height && y == 0 = [(height - 1, 0)]
  | x == 0 = [(0, y - 1), (0, y)]
  | x == height = [(height - 1, y - 1), (height - 1, y)]
  | y == 0 = [(x - 1, 0), (x, 0)]
  | y == width = [(x - 1, width - 1), (x, width - 1)]
  | otherwise = [(xarg, yarg) | xarg <- [(x - 1) .. x], yarg <- [(y - 1) .. y]]
