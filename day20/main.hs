{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Bifunctor (bimap, first, second)
import Data.Function (on)
import Data.List (intercalate, minimumBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Debug.Trace (trace)
import Utils

type Binary = [Char]

type Point = (Int, Int)

type Image = Map Point Char

printImage image =
  let ps = M.keys image
      minX = ps $> map fst .> minimum
      minY = ps $> map snd .> minimum
      maxX = ps $> map fst .> maximum
      maxY = ps $> map snd .> maximum
      coords = [(x, y) | y <- [minY .. maxY], x <- [minX .. maxX]]
      charAtPoint p =
        M.findWithDefault '0' p image
          $> \case '1' -> '#'; '0' -> '.'; other -> other
      withSepLine lst = lst ++ [putStrLn "---"]
   in [minY .. maxY] $> map (\y -> putStrLn ([minX .. maxX] $> map (\x -> charAtPoint (x, y))))
        .> withSepLine
        .> sequence_

neighbors :: Image -> Point -> Binary
neighbors image p@(x, y) =
  let diffs = [(dx, dy) | dy <- [-1, 0, 1], dx <- [-1, 0, 1]]
      lookup p = M.findWithDefault '0' p image
      indices = diffs $> map (bimap (x +) (y +))
   in indices $> map lookup

computeNewValue algo image p =
  let bin = neighbors image p
      index = bin $> binaryToInt
   in algo !! index

process :: [Char] -> Image -> Image
process algo image =
  M.foldrWithKey (\p _ acc -> M.insert p (computeNewValue algo image p) acc) M.empty image

enlargeBoundsOnceWith c image =
  let ps = M.keys image
      minX = ps $> map fst .> minimum
      minY = ps $> map snd .> minimum
      maxX = ps $> map fst .> maximum
      maxY = ps $> map snd .> maximum
      newPs =
        [(x, y) | x <- [minX - 1], y <- [minY - 1 .. maxY + 1]]
          ++ [(x, y) | x <- [maxX + 1], y <- [minY - 1 .. maxY + 1]]
          ++ [(x, y) | x <- [minX - 1 .. maxX + 1], y <- [minY - 1]]
          ++ [(x, y) | x <- [minX - 1 .. maxX + 1], y <- [maxY + 1]]
   in foldr (`M.insert` c) image newPs

enlargeBoundsWith c n image =
  iterate (enlargeBoundsOnceWith c) image !! n

reduceBounds n image =
  let ps = M.keys image
      minX = ps $> map fst .> minimum
      minY = ps $> map snd .> minimum
      maxX = ps $> map fst .> maximum
      maxY = ps $> map snd .> maximum
   in M.filterWithKey (\(x, y) _ -> x - minX >= n && maxX - x >= n && y - minY >= n && maxY - y >= n) image

main :: IO ()
main = do
  -- config constants
  let iters = 50
  let reduceN = iters
  let enlargeN = 2 * iters + 10

  contents <- getContents
  let [algoArr, imageArr] = contents $> map (\case '#' -> '1'; '.' -> '0'; other -> other) .> lines .> splitOn null
  let algo = concat algoArr
  let (xSize, ySize) = (length $ head imageArr, length imageArr)
  let coords = [(x, y) | y <- [0 .. ySize -1], x <- [0 .. xSize - 1]]
  let image = zip coords (concat imageArr) $> M.fromList .> enlargeBoundsWith '0' enlargeN
  let computation = iterate (process algo) image

  let results = take (iters + 1) computation

  let lastImageReduced = reduceBounds reduceN (last results)
  printImage lastImageReduced

  let e1Result = lastImageReduced $> M.toList .> map snd .> filter (== '1') .> length
  print e1Result
