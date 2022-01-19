module Utils
  ( removeElements,
  )
where

removeElements :: Eq a => [a] -> [a] -> [a]
removeElements toDelete = filter (`notElem` toDelete)