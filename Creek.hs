module Creek (Creek) where

data Creek = Creek (Int, Int) [((Int, Int), Int)] deriving (Read, Show)