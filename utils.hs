module Utils where

import Control.Applicative (ZipList (ZipList, getZipList))

(.>) = flip (.)

($>) = flip ($)

infixr 6 $>

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s = case dropWhile p s of
  "" -> []
  s' -> w : splitOn p s''
    where
      (w, s'') = break p s'

transpose :: [[a]] -> [[a]]
transpose = getZipList . traverse ZipList

mapWithIndex :: (Num a1, Enum a1) => (a2 -> b) -> [a2] -> [(a1, b)]
mapWithIndex f xs = zip [0 ..] (map f xs)

removeAt :: [Int] -> [b] -> [b]
removeAt is xs = mapWithIndex id xs $> filter (\(i', _) -> i' `notElem` is) .> map snd
