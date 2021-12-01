module Main where

import Utils

diff :: [Int] -> [Int]
diff [] = []
diff xs = zipWith (-) (tail xs) xs

e1 :: [Int] -> Int
e1 = diff .> filter (> 0) .> length

e2 :: [Int] -> Int
e2 ints =
  zip3 ints (tail ints) (tail $ tail ints)
    $> map (\(x, y, z) -> x + y + z)
    .> e1

main :: IO ()
main =
  do
    contents <- getContents
    let ints = map read (lines contents)
    e1 ints $> print
    e2 ints $> print
