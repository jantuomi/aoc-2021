module Main where

import Control.Applicative (ZipList (ZipList, getZipList))
import Data.Char (digitToInt)
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Utils

convert :: [Int] -> Int
-- Convert binary (list of zeros and ones) to decimal
convert [] = 0
convert (x : xs) = x + 2 * convert xs

toIntMatrix :: [[Char]] -> [[Int]]
toIntMatrix = map (map digitToInt)

mostCommon :: [Int] -> Int
-- Appending [0, 1] is a hack to make maximumBy choose 1 in a tie due to semantics
mostCommon lst = maximumBy (comparing (\x -> length $ filter (x ==) lst)) (lst ++ [0, 1])

leastCommon :: [Int] -> Int
-- Prepending [0, 1] is a hack to make minimumBy choose 0 in a tie due to semantics
leastCommon lst = minimumBy (comparing (\x -> length $ filter (x ==) lst)) ([0, 1] ++ lst)

e1 :: [String] -> Int
e1 input =
  let mtx = toIntMatrix input $> transpose
      gamma = mtx $> map mostCommon .> reverse .> convert
      epsilon = mtx $> map leastCommon .> reverse .> convert
   in gamma * epsilon

recur :: ([Int] -> Int) -> [[Int]] -> Int -> Int
-- Recurse through the matrix from left to right (i = [0..columnN - 1]) and filter out
-- rows not matching the criteria. If filter results in only one resulting value, stop recursion.
recur criteriaFn mtx i =
  let column = map (!! i) mtx
      crit = criteriaFn column
      filtered = filter (\row -> row !! i == crit) mtx
   in if length filtered == 1
        then convert $ reverse $ head filtered
        else recur criteriaFn filtered (i + 1)

e2 :: [String] -> Int
e2 input =
  let mtx = input $> toIntMatrix
      oxyr = recur mostCommon mtx 0
      co2r = recur leastCommon mtx 0
   in oxyr * co2r

main :: IO ()
main =
  do
    contents <- getContents
    let input =
          contents
            $> lines

    e1 input $> print
    e2 input $> print
