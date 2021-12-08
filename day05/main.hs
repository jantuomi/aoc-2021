module Main where

import qualified Data.Map as Map
import Data.Text (pack, replace, split, unpack)
import Debug.Trace (trace, traceShow)
import Utils

data Line = Line
  { start :: (Int, Int),
    end :: (Int, Int)
  }
  deriving (Show)

line :: (Int, Int) -> (Int, Int) -> Line
line start end = Line {start = start, end = end}

rowToLine :: String -> Line
rowToLine row =
  pack row
    $> replace (pack " -> ") (pack ",")
    .> split (== ',')
    .> map unpack
    .> map read
    .> (\[a, b, c, d] -> line (a, b) (c, d))

markCovered :: (Int, Int) -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
markCovered p = Map.insertWith (+) p 1

range :: Int -> Int -> [Int]
range x y
  | x < y = [x .. y]
  | x > y = reverse [y .. x]
  | x == y = [x]
  | otherwise = error "unreachable"

lineToPoints1 :: Line -> [(Int, Int)]
lineToPoints1 l@(Line (x1, y1) (x2, y2))
  | isHorizOrVert l =
    let xs = range x1 x2
        ys = range y1 y2
     in [(x, y) | x <- xs, y <- ys]
  | otherwise = []

isHorizOrVert :: Line -> Bool
isHorizOrVert (Line (x1, y1) (x2, y2)) =
  x1 == x2 || y1 == y2

e1 ls =
  let hmap = Map.empty
      processed = processLines lineToPoints1 ls hmap
      nDangerous = Map.elems processed $> filter (>= 2) .> length
   in nDangerous

lineToPoints2 :: Line -> [(Int, Int)]
lineToPoints2 l@(Line (x1, y1) (x2, y2))
  | isHorizOrVert l = lineToPoints1 l
  | otherwise =
    let xs = range x1 x2
        ys = range y1 y2
     in zip xs ys

processLines :: (Line -> [(Int, Int)]) -> [Line] -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
processLines _ [] hmap = hmap
processLines lineToPointsFn (l : ls) hmap =
  let points = lineToPointsFn l
      updatedHmap = foldr markCovered hmap points
   in processLines lineToPointsFn ls updatedHmap

e2 ls =
  let hmap = Map.empty
      processed = processLines lineToPoints2 ls hmap
   in Map.elems processed $> filter (>= 2) .> length

main :: IO ()
main =
  do
    contents <- getContents
    let ls = contents $> lines .> map rowToLine

    e1 ls $> print
    e2 ls $> print
