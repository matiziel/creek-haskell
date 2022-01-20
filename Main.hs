import Board
import Data.List
import Rules
import System.IO
import Utils

data Creek = Creek BoardSize Rules deriving (Read, Show)

main = do
  putStrLn "Podaj ścieżkę do pliku:"
  line <- getLine
  contents <- readFile line
  -- contents <- readFile "./test_files/test5.txt"
  let creek = read contents :: Creek
  let board = generateBoard (getBoardSize creek)
  let rules = getRules creek
  let knownSolved = solveKnownCells board rules
  let restRules = removeAppliedKnownRules knownSolved rules
  let patternsCombination = getPatternsCombinationForRules knownSolved restRules
  let solved = solveRest knownSolved restRules
  print solved

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
getCellsForRuleApply (Board (height, width) cells) ((x, y), val) =
  [ (xarg, yarg)
    | xarg <- [(x - 1) .. x],
      xarg >= 0,
      xarg < height,
      yarg <- [(y - 1) .. y],
      yarg >= 0,
      yarg < width
  ]

removeAppliedKnownRules :: Board -> Rules -> Rules
removeAppliedKnownRules b rules = cornerRulesWithOne
  where
    rulesWithZeroAndFour = filter (not . isRuleValueIn [0, 4]) rules
    edgeRulesWithTwo = filter (not . isEdgeKnownRule b) rulesWithZeroAndFour
    cornerRulesWithOne = filter (not . isCornerKnownRule b) edgeRulesWithTwo

solveRest :: Board -> Rules -> Maybe Board
solveRest b rules =
  case solve (Just b) (getSequenceOfPatterns b rules) rules of
    Nothing -> Nothing
    Just board -> Just (replaceUknownCells board)

solve :: Maybe Board -> [Patterns] -> Rules -> Maybe Board
solve Nothing _ _ = Nothing
solve _ [] _ = Nothing
solve (Just b) (p : ps) rules =
  case newBoard of
    Nothing -> solve (Just b) ps rules
    Just board -> Just board
  where
    newBoard = applyPatternsWithValidations b p rules

applyPatternsWithValidations :: Board -> Patterns -> Rules -> Maybe Board
applyPatternsWithValidations b patterns rules =
  case newBoard of
    Nothing -> Nothing
    Just board ->
      if validateRules board rules && validateCreek board
        then Just board
        else Nothing
  where
    newBoard = applyPatterns b patterns rules

applyPatterns :: Board -> Patterns -> Rules -> Maybe Board
applyPatterns b [] [] = Just b
applyPatterns _ [] _ = Nothing
applyPatterns _ _ [] = Nothing
applyPatterns b (p : ps) (r : rs) =
  case applyPattern b p r of
    Nothing -> Nothing
    Just newBoard -> applyPatterns newBoard ps rs

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

getPattersForRule :: Board -> Rule -> Patterns
getPattersForRule b@(Board (height, width) _) r@(_, val) =
  combination val (getCellsForRuleApply b r)

getPatternsCombinationForRules :: Board -> Rules -> [Patterns]
getPatternsCombinationForRules b = map (getPattersForRule b)

getSequenceOfPatterns :: Board -> Rules -> [Patterns]
getSequenceOfPatterns b rules = sequence patterns
  where
    patterns = getPatternsCombinationForRules b rules