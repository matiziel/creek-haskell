module Rules
  ( Rule,
    Rules,
    Pattern,
    Patterns,
  )
where

type Rule = ((Int, Int), Int)

type Rules = [Rule]

type Pattern = [(Int, Int)]

type Patterns = [Pattern]