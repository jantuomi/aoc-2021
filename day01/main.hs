module Main where

import Utils

diff :: [Int] -> [Int]
diff [] = []
diff xs = zipWith (-) (tail xs) xs

e1 :: [Int] -> IO ()
e1 ints = do
  let result = ints $> diff .> filter (> 0) .> length
  print result

e2 :: [Int] -> IO ()
e2 ints = do
  let result =
        zip3 ints (tail ints) (tail (tail ints))
          $> map (\(x, y, z) -> x + y + z)
          .> diff
          .> filter (> 0)
          .> length

  print result

main :: IO ()
main =
  do
    contents <- getContents
    let ints = map read (lines contents)
    e1 ints
    e2 ints
