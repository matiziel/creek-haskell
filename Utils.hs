module Utils
  ( removeElements,
    combination,
  )
where

import Data.List

removeElements :: Eq a => [a] -> [a] -> [a]
removeElements toDelete = filter (`notElem` toDelete)

combination :: Int -> [a] -> [[a]]
combination 0 lst = [[]]
combination n lst = do
  (x : xs) <- tails lst
  rest <- combination (n -1) xs
  return $ x : rest