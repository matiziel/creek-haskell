import Board
import Data.List
import Distribution.TestSuite (Result (Error))
import Rules
import System.IO
import Utils

data Creek = Creek BoardSize Rules deriving (Read, Show)

main = do
  -- putStrLn "Podaj ścieżkę do pliku:"
  -- line <- getLine
  -- contents <- readFile line
  contents <- readFile "./test_files/test2.txt"
  let creek = read contents :: Creek
  let board = generateBoard (getBoardSize creek)
  let rules = getRules creek
  let knownSolved = solveKnownCells board rules
  let restRules = removeAppliedKnownRules knownSolved rules
  print (solveRest knownSolved restRules)

getBoardSize :: Creek -> BoardSize
getBoardSize (Creek size _) = size

getRules :: Creek -> Rules
getRules (Creek _ rules) = rules

solveKnownCells :: Board -> Rules -> Board
solveKnownCells b@(Board (height, width) cells) rules = filledCornersCells
  where
    rulesWithZero = filter (isRuleValueIn [0]) rules
    emptyCells = fillCells b rulesWithZero Empty
    rulesWithFour = filter (isRuleValueIn [4]) rules
    filledCells = fillCells emptyCells rulesWithFour Filled
    edgeRulesWithTwo = filter (isEdgeKnownRule b) rules
    filledEdgesCells = fillCells filledCells edgeRulesWithTwo Filled
    cornerRulesWithOne = filter (isCornerKnownRule b) rules
    filledCornersCells = fillCells filledEdgesCells cornerRulesWithOne Filled

isRuleValueIn :: [Int] -> Rule -> Bool
isRuleValueIn values ((a, b), c) = c `elem` values

isEdgeKnownRule :: Board -> Rule -> Bool
isEdgeKnownRule (Board (height, width) _) ((x, y), val) =
  (val == 2)
    && ( ((x == 0 || x == height) && (y > 0 && y < width))
           || ((y == 0 || y == width) && (x > 0 || x < height))
       )

isCornerKnownRule :: Board -> Rule -> Bool
isCornerKnownRule (Board (height, width) _) ((x, y), val) =
  (val == 1)
    && ( (x == 0 && (y == 0 || y == width))
           || (x == height && (y == 0 || y == width))
       )

fillCells :: Board -> Rules -> BoardCell -> Board
fillCells board [] _ = board
fillCells b (x : xs) value =
  fillCells (applyRuleToBoard b x value) xs value

applyRuleToBoard :: Board -> Rule -> BoardCell -> Board
applyRuleToBoard b rule value = setBoardCells b [(index, value) | index <- getCellsForRuleApply b rule]

getCellsForRuleApply :: Board -> Rule -> [(Int, Int)]
getCellsForRuleApply (Board (height, width) cells) ((x, y), val)
  | x == 0 && y == 0 = [(0, 0)]
  | x == height && y == height = [(height - 1, width - 1)]
  | x == 0 && y == width = [(0, y - 1)]
  | x == height && y == 0 = [(height - 1, 0)]
  | x == 0 = [(0, y - 1), (0, y)]
  | x == height = [(height - 1, y - 1), (height - 1, y)]
  | y == 0 = [(x - 1, 0), (x, 0)]
  | y == width = [(x - 1, width - 1), (x, width - 1)]
  | otherwise = [(xarg, yarg) | xarg <- [(x - 1) .. x], yarg <- [(y - 1) .. y]]

removeAppliedKnownRules :: Board -> Rules -> Rules
removeAppliedKnownRules b rules = cornerRulesWithOne
  where
    rulesWithZeroAndFour = filter (not . isRuleValueIn [0, 4]) rules
    edgeRulesWithTwo = filter (not . isEdgeKnownRule b) rulesWithZeroAndFour
    cornerRulesWithOne = filter (not . isCornerKnownRule b) edgeRulesWithTwo

-- solveRule :: Board -> Rule -> Board
-- solveRule b@(Board (height, width) cells) ((x, y), 1) = b
-- solveRule b@(Board (height, width) cells) ((x, y), 2) = b
-- solveRule b@(Board (height, width) cells) ((x, y), 3) = b
-- solveRule _ _ = error "Cannot apply this type of rule"

solveRest :: Board -> Rules -> Maybe Board
solveRest b rules = solve (Just b) rules (getPattersForNextRule b rules) rules

solve :: Maybe Board -> Rules -> Patterns -> Rules -> Maybe Board
solve Nothing _ _ _ = Nothing
solve (Just b) [] _ allRules =
  if validateRules b allRules
    then Just b
    else Nothing
solve (Just b) _ [] allRules =
  if validateRules b allRules
    then Just b
    else Nothing
solve (Just b) (r : rs) (p : ps) allRules =
  case solve newBoard rs patterns allRules of
    Nothing -> solve (Just b) (r : rs) ps allRules
    Just solvedBoard -> Just solvedBoard
  where
    newBoard = applyPattern b p r
    patterns = getPattersForNextRule b rs

applyPattern :: Board -> Pattern -> Rule -> Maybe Board
applyPattern b p r =
  if canApplyPattern b p && validateRule newBoard r
    then Just newBoard
    else Nothing
  where
    newBoard = setBoardCells b [(index, Filled) | index <- p]

validateRule :: Board -> Rule -> Bool
validateRule b r@(_, val) =
  val == getSumOfFilledCells b (getCellsForRuleApply b r)

validateRules :: Board -> Rules -> Bool
validateRules b = foldr ((&&) . validateRule b) True

getSumOfFilledCells :: Board -> [(Int, Int)] -> Int
getSumOfFilledCells _ [] = 0
getSumOfFilledCells b (x : xs) =
  if boardCellValueAt b x == Filled
    then 1 + getSumOfFilledCells b xs
    else getSumOfFilledCells b xs

canApplyPattern :: Board -> [(Int, Int)] -> Bool
canApplyPattern (Board (height, width) _) [] = True
canApplyPattern b (p : ps) =
  (boardCellValueAt b p /= Empty) && canApplyPattern b ps

getPattersForNextRule :: Board -> Rules -> Patterns
getPattersForNextRule _ [] = []
getPattersForNextRule b@(Board (height, width) _) (r@(_, val) : rs)
  | val == 1 = getPattersForRuleWith1 b r
  | val == 2 = getPattersForRuleWith2 r
  | val == 3 = getPattersForRuleWith3 r
  | otherwise = error "Wrong rule to get pattern"

getPattersForRuleWith1 :: Board -> Rule -> [[(Int, Int)]]
getPattersForRuleWith1 (Board (height, width) _) ((x, y), _)
  | x == 0 = [[(x, y - 1)], [(x, y)]]
  | y == 0 = [[(x - 1, y)], [(x, y)]]
  | x == height = [[(x - 1, y - 1)], [(x - 1, y)]]
  | y == height = [[(x - 1, y - 1)], [(x, y - 1)]]
  | otherwise =
    [ [(x - 1, y - 1)],
      [(x - 1, y - 1)],
      [(x - 1, y - 1)],
      [(x, y - 1)]
    ]

getPattersForRuleWith2 :: Rule -> [[(Int, Int)]]
getPattersForRuleWith2 ((x, y), _) =
  [ [(x - 1, y - 1), (x - 1, y)],
    [(x - 1, y - 1), (x, y - 1)],
    [(x - 1, y - 1), (x, y)],
    [(x, y - 1), (x, y)],
    [(x - 1, y), (x, y)],
    [(x, y - 1), (x - 1, y)]
  ]

getPattersForRuleWith3 :: Rule -> [[(Int, Int)]]
getPattersForRuleWith3 ((x, y), _) =
  [ [(x - 1, y - 1), (x - 1, y)],
    [(x - 1, y - 1), (x, y - 1)],
    [(x - 1, y - 1), (x, y)],
    [(x, y - 1), (x, y)]
  ]
