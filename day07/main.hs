module Main where

import Data.Text (pack, split, unpack)
import Utils

move1 :: Int -> Int -> Int
move1 target crab = abs (crab - target)

move2 :: Int -> Int -> Int
move2 target crab =
  let n = abs (crab - target)
   in n * (n + 1) `div` 2

makeMoves moveFn crabs target = foldr (\crab acc -> acc + moveFn target crab) 0 crabs

e :: (Int -> Int -> Int) -> [Int] -> Int
e moveFn crabs =
  let targets = [minimum crabs .. maximum crabs]
      consumptions = map (makeMoves moveFn crabs) targets
   in minimum consumptions

main :: IO ()
main =
  do
    contents <- getContents
    let input = contents $> pack .> split (== ',') .> map (unpack .> read) :: [Int]

    e move1 input $> print
    e move2 input $> print
