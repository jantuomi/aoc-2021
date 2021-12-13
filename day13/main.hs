{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Bifunctor (bimap, first, second)
import Data.List (foldl', nub)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (isPrefixOf, pack, split, unpack)
import Utils

type Point = (Int, Int)

data Fold = LeftFold Int | UpFold Int deriving (Show)

parseFold :: String -> Fold
parseFold s =
  let (coord, num) = drop 11 s $> splitToPair (== '=')
   in if coord == "x"
        then LeftFold (read num)
        else UpFold (read num)

foldLeft :: Int -> [Point] -> [Point]
foldLeft yLine = map (first (\x -> min x $ yLine + (yLine - x))) .> nub

foldUp :: Int -> [Point] -> [Point]
foldUp xLine = map (second (\y -> min y $ xLine + (xLine - y))) .> nub

doFold points fold = case fold of
  LeftFold yLine -> foldLeft yLine points
  UpFold xLine -> foldUp xLine points

e1 :: [Point] -> [Fold] -> [Point]
e1 points folds =
  doFold points (head folds)

printCode :: [Point] -> IO ()
printCode points =
  let pointsSet = Set.fromList points
      xSize = maximum (map fst points) + 1
      ySize = maximum (map snd points) + 1
      pointToChar p = if p `elem` pointsSet then '#' else '.'
      rowStrs = [0 .. ySize - 1] $> map (\y -> [0 .. xSize - 1] $> map (\x -> pointToChar (x, y)))
   in rowStrs $> map putStrLn .> sequence_

e2 =
  foldl' doFold

main :: IO ()
main =
  do
    contents <- getContents

    let input =
          contents
            $> lines
            .> break null
    let pointsStr = fst input
    let foldsStr = snd input $> drop 1

    let points = pointsStr $> map (splitToPair (== ',') .> bimap read read) :: [Point]
    let folds = foldsStr $> map parseFold

    let result1 = e1 points folds
    result1 $> length .> print
    let result2 = e2 points folds
    result2 $> length .> print
    result2 $> printCode
