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

getBoardSize :: Creek -> BoardSize
getBoardSize (Creek size _) = size

getRules :: Creek -> Rules
getRules (Creek _ rules) = rules

solve :: Board -> Rules -> Board
solve b@(Board (height, width) cells) rules = b

fillKnownEmptyCells :: Board -> Rules -> Board
fillKnownEmptyCells b@(Board (height, width) cells) rules = b

filterRules :: Rule -> Int -> Bool
filterRules r@((a, b), c) value = c == value