module Utils where

import Control.Applicative (ZipList (ZipList, getZipList))
import Data.Char (digitToInt)

(.>) = flip (.)

($>) = flip ($)

infixr 6 $>

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p s = case dropWhile p s of
  [] -> []
  s' -> w : splitOn p s''
    where
      (w, s'') = break p s'

transpose :: [[a]] -> [[a]]
transpose = getZipList . traverse ZipList

mapWithIndex :: (a -> b) -> [a] -> [(Int, b)]
mapWithIndex f xs = zip [0 ..] (map f xs)

removeAt :: [Int] -> [b] -> [b]
removeAt is xs = mapWithIndex id xs $> filter (\(i', _) -> i' `notElem` is) .> map snd

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first, rest) = splitAt n list

splitToPair :: (Show a) => (a -> Bool) -> [a] -> ([a], [a])
splitToPair f lst =
  let x' = takeWhile (not . f) lst
      y' = dropWhile (not . f) lst $> drop 1
   in (x', y')

count :: (a -> Bool) -> [a] -> Int
count pred xs = xs $> filter pred .> length

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x : xs) = x : if p x then takeUntil p xs else []

fromSymEither (Left x) = x
fromSymEither (Right x) = x

-- | Convert binary (list of zeros and ones) to decimal
binaryToInt :: String -> Int
binaryToInt = reverse .> binaryToIntRev

binaryToIntRev [] = 0
binaryToIntRev (x : xs) = digitToInt x + 2 * binaryToIntRev xs
