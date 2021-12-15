module Main where

import Control.Monad (foldM)
import Data.Bifunctor (bimap, first, second)
import Data.Char (digitToInt)
import Data.Function (on)
import qualified Data.List as L
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQ
import Debug.Trace (traceShow, traceShowId)
import Utils

type Point = (Int, Int)

type Dim = (Int, Int)

type WMap = Map Point Int

type TMap = Map Point Int

update :: WMap -> TMap -> MinPQueue Int Point -> Point -> Point -> (TMap, MinPQueue Int Point)
update wmap tmap unvisitedPQ source target =
  let w = wmap ! target
      oldT = tmap ! target
      newT = tmap ! source + w
   in if newT < oldT
        then (M.insert target (min oldT newT) tmap, PQ.insert newT target unvisitedPQ)
        else (tmap, unvisitedPQ)

getNeighbors :: WMap -> Point -> [Point]
getNeighbors wmap p@(x, y) =
  [(1, 0), (0, 1), (-1, 0), (0, -1)]
    $> map (bimap (x +) (y +))
    .> filter (`M.member` wmap)

recurse :: Point -> WMap -> TMap -> MinPQueue Int Point -> Int
recurse goal wmap tmap unvisitedPQ
  | null unvisitedPQ = tmap ! goal
  | otherwise =
    let ((currentT, current), unvisitedPQ') = PQ.deleteFindMin unvisitedPQ

        -- if currentT == tmap ! current
        unvisitedNeighbors = getNeighbors wmap current
        (tmap', unvisitedPQ'') =
          L.foldl' (\(tm_, pq_) nb -> update wmap tm_ pq_ current nb) (tmap, unvisitedPQ') unvisitedNeighbors
     in if currentT > tmap ! current
          then recurse goal wmap tmap unvisitedPQ'
          else recurse goal wmap tmap' unvisitedPQ''

dijkstra :: Point -> WMap -> Point -> Int
dijkstra goal costs start =
  let initTMap = M.mapWithKey (\_ _ -> maxBound :: Int) costs $> M.adjust (const 0) start
      initUnvisited = PQ.fromList [(0, start)]
   in recurse goal costs initTMap initUnvisited

e1 chart (xSize, ySize) =
  let start = (0, 0)
      goal = (xSize - 1, ySize - 1)
   in dijkstra goal chart start

e2 chart (xSize, ySize) =
  let start = (0, 0)
      goal = (5 * xSize - 1, 5 * ySize - 1)
      chunks = [(cx, cy) | cx <- [0 .. 4], cy <- [0 .. 4]]
      chart' =
        chunks
          $> foldr
            ( \(cx, cy) acc ->
                M.foldrWithKey
                  ( \(x, y) v ->
                      M.insert (x + cx * xSize, y + cy * ySize) ((v + cx + cy - 1) `mod` 9 + 1)
                  )
                  acc
                  chart
            )
            M.empty
   in dijkstra goal chart' start

main :: IO ()
main =
  do
    contents <- getContents

    let input = contents $> lines .> map (map digitToInt)
    let dim@(xSize, ySize) = (length $ head input, length input)
    let coords = [(x, y) | y <- [0 .. ySize - 1], x <- [0 .. xSize - 1]]
    let chart = zip coords (concat input) $> M.fromList

    e1 chart dim $> print
    e2 chart dim $> print
