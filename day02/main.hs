module Main where

import Data.Foldable (Foldable (foldl'))
import Utils

toDelta :: (String, Int) -> (Int, Int)
toDelta ("up", n) = (0, - n)
toDelta ("down", n) = (0, n)
toDelta ("forward", n) = (n, 0)
toDelta x = error $ "invalid instruction: " ++ show x

e1 :: [(String, Int)] -> Int
e1 =
  map toDelta
    .> foldl' (\(ax, ay) (x, y) -> (ax + x, ay + y)) (0, 0)
    .> uncurry (*)

e2 :: [(String, Int)] -> Int
e2 =
  map toDelta
    .> foldl' (\(ax, ay, aaim) (x, aim) -> (ax + x, ay + aaim * x, aaim + aim)) (0, 0, 0)
    .> \(x, y, _) -> x * y

main :: IO ()
main =
  do
    contents <- getContents
    let input =
          contents
            $> lines
            .> map words
            .> map (\[x, y] -> (x, read y))

    e1 input $> print
    e2 input $> print
