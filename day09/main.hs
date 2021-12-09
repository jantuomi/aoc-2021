module Main where

import Data.Char (digitToInt)
import Data.List (foldl', sortOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Utils

neighbors :: (Int, Int) -> (Int, Int) -> [(Int, Int) -> (Int, Int)]
neighbors (xsize, ysize) (x, y)
  | x == 0 && y == 0 = [down, right]
  | x == 0 && y == ysize - 1 = [up, right]
  | x == xsize - 1 && y == 0 = [down, left]
  | x == xsize - 1 && y == ysize - 1 = [up, left]
  | x == 0 = [down, right, up]
  | y == 0 = [left, down, right]
  | x == xsize - 1 = [up, left, down]
  | y == ysize - 1 = [left, up, right]
  | otherwise = [up, left, down, right]

up (x, y) = (x, y - 1)

right (x, y) = (x + 1, y)

down (x, y) = (x, y + 1)

left (x, y) = (x - 1, y)

isLowPoint :: Map (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Bool
isLowPoint hmap size@(xsize, ysize) p@(x, y) =
  let dirs = neighbors size p
   in all (\dir -> (hmap ! dir p) > (hmap ! p)) dirs

e1 :: Map (Int, Int) Int -> (Int, Int) -> Int
e1 hmap size =
  let points = Map.keys hmap
      lowPoints = points $> filter (isLowPoint hmap size)
      riskLevels = lowPoints $> map (hmap !) .> map (+ 1) .> sum
   in riskLevels

walk :: Map (Int, Int) Int -> (Int, Int) -> Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
walk hmap size visited p =
  let dirs = neighbors size p
      newVisited = Set.insert p visited
      upslopes =
        dirs $> map (\dir -> dir p)
          .> filter (`notElem` newVisited)
          .> filter (\slopep -> hmap ! slopep /= 9)
          .> filter (\slopep -> (hmap ! slopep) > (hmap ! p))
      upslopeResults = upslopes $> map (walk hmap size newVisited)
   in foldl' Set.union newVisited upslopeResults

basin :: Map (Int, Int) Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
basin hmap size p =
  walk hmap size Set.empty p $> Set.toList

e2 :: Map (Int, Int) Int -> (Int, Int) -> Int
e2 hmap size =
  let points = Map.keys hmap
      lowPoints = points $> filter (isLowPoint hmap size)
      basins = lowPoints $> map (basin hmap size)
      basinsSorted = sortOn length basins $> reverse
      threeLargest = basinsSorted $> take 3
   in threeLargest $> map length .> product

main :: IO ()
main =
  do
    contents <- getContents

    let input = contents $> lines .> map (map digitToInt)
    let ysize = length input
    let xsize = length (head input)
    let coords = [(x, y) | y <- [0 .. ysize -1], x <- [0 .. xsize - 1]]
    let inputFlat = concat input
    let hmap = Map.fromList (zip coords inputFlat)

    e1 hmap (xsize, ysize) $> print
    e2 hmap (xsize, ysize) $> print
