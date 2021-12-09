module Main where

import Data.List (find, permutations, sort)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import Data.Text (intercalate, pack, replace, split, unpack)
import Utils

processLine =
  pack
    .> replace (pack " | ") (pack " ")
    .> unpack
    .> words
    .> (\lst -> (slice 0 9 lst, slice 10 13 lst))

e1 :: [([String], [String])] -> Int
e1 input =
  let outputs = map snd input
      outputsFlat = concat outputs
      easyOutputs = filter (\s -> length s `elem` [2, 4, 3, 7]) outputsFlat
   in length easyOutputs

symbols = "abcdefg"

allSolutions :: [Map Char Char]
allSolutions = permutations symbols $> map (zip symbols) .> map Map.fromList

segmentsToDigit :: [Char] -> Maybe Char
segmentsToDigit "abcefg" = Just '0'
segmentsToDigit "cf" = Just '1'
segmentsToDigit "acdeg" = Just '2'
segmentsToDigit "acdfg" = Just '3'
segmentsToDigit "bcdf" = Just '4'
segmentsToDigit "abdfg" = Just '5'
segmentsToDigit "abdefg" = Just '6'
segmentsToDigit "acf" = Just '7'
segmentsToDigit "abcdefg" = Just '8'
segmentsToDigit "abcdfg" = Just '9'
segmentsToDigit other = Nothing

toSegments :: Map Char Char -> [Char] -> [Char]
toSegments hmap signal = signal $> map (hmap !)

isSignalValidDigit :: Map Char Char -> [Char] -> Bool
isSignalValidDigit hmap signal =
  let segs = toSegments hmap signal $> sort
   in segmentsToDigit segs $> isJust

isValid :: Map Char Char -> [String] -> Bool
isValid hmap = all (isSignalValidDigit hmap)

processRow :: ([String], [String]) -> Int
processRow row =
  let signals = fst row
      outputs = snd row
      solutionM = find (`isValid` signals) allSolutions
      solution = fromJust solutionM
      outputDigits = outputs $> map (toSegments solution .> sort .> segmentsToDigit .> fromJust)
   in read outputDigits

e2 rows =
  let outputNums = map processRow rows
   in sum outputNums

main :: IO ()
main =
  do
    contents <- getContents

    let input = contents $> lines .> map processLine

    e1 input $> print
    e2 input $> print
