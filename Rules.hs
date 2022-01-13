module Rules
  ( Rule,
    Rules,
  )
where

type Rule = ((Int, Int), Int)

type Rules = [Rule]