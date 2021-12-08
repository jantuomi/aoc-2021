module Main where

import Data.Map ((!))
import qualified Data.Map as Map
import Data.Text (pack, split, unpack)
import Utils

e1 :: Int -> [Int] -> Int
e1 n fish
  | n == 0 = length fish
  | otherwise =
    let updatedNested = map (\n -> if n == 0 then [6, 8] else [n - 1]) fish
        updated = concat updatedNested
     in e1 (n - 1) updated

update :: Map.Map Int Int -> Int -> Int -> Int
update hmap 8 _ = hmap ! 0
update hmap 6 _ = (hmap ! 0) + (hmap ! 7)
update hmap n _ = hmap ! (n + 1)

e2 :: Int -> Map.Map Int Int -> Int
e2 n hmap
  | n == 0 = sum (Map.elems hmap)
  | otherwise =
    let newHmap = Map.mapWithKey (update hmap) hmap
     in e2 (n - 1) newHmap

main :: IO ()
main =
  do
    contents <- getContents
    let initial = contents $> pack .> split (== ',') .> map (unpack .> read) :: [Int]

    e1 80 initial $> print
    let hmapInit = Map.fromList [(k, v) | k <- [0 .. 8], v <- [0]]
    let hmap = foldr (\fish -> Map.insertWith (+) fish 1) hmapInit initial
    e2 256 hmap $> print
