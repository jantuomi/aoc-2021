module Main where

import Data.Char (isLower, isUpper)
import Data.Function (on)
import Data.List (concatMap, groupBy, intercalate, nub, sort, sortOn)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Text (pack, split, unpack)
import Debug.Trace (traceShow)
import Utils

newtype Cave = Cave String

instance Eq Cave where
  (==) (Cave a) (Cave b) = a == b

instance Ord Cave where
  compare (Cave a) (Cave b) = compare a b

instance Show Cave where
  show (Cave cave) = show cave

newtype Graph = Graph (Map Cave [Cave])

instance Show Graph where
  show (Graph hmap) = show hmap

newtype Path = Path [Cave]

instance Show Path where
  show (Path caves) = caves $> map (\(Cave s) -> s) .> intercalate ","

neighbors (Graph hmap) c = hmap ! c

groupCavePairs :: [(String, String)] -> [(Cave, [Cave])]
groupCavePairs =
  sortOn fst
    .> groupBy ((==) `on` fst)
    .> map (\lst -> (Cave $ fst (head lst), map (snd .> Cave) lst))

toPair [a, b] = (a, b)
toPair other = error $ "invalid list: " ++ show other

flipPair (a, b) = (b, a)

isSmall (Cave cave) = isLower (head cave)

isBig (Cave cave) = isUpper (head cave)

isStart (Cave cave) = cave == "start"

isEnd (Cave cave) = cave == "end"

walk1 :: Graph -> Path -> Cave -> [Path]
walk1 graph (Path path) c
  | isEnd c = [Path $ reverse (c : path)]
  | c `elem` path && isSmall c = []
  | otherwise =
    let path' = Path $ c : path
        ns = neighbors graph c
     in concatMap (walk1 graph path') ns

e1 :: Graph -> [Path]
e1 graph = walk1 graph (Path []) (Cave "start")

countBy f = filter f .> length

count lst el = filter (== el) lst $> length

walk2 :: Graph -> Path -> Bool -> Cave -> Int
walk2 graph p@(Path path) smallVisitedTwice c@(Cave cName)
  | isEnd c = 1
  | isStart c && c `elem` path = 0
  | isSmall c && c `elem` path && smallVisitedTwice = 0
  | isSmall c && c `elem` path && not smallVisitedTwice =
    let path' = Path $ c : path
        ns = neighbors graph c
     in sum $ map (walk2 graph path' True) ns
  | otherwise =
    let path' = Path $ c : path
        ns = neighbors graph c
     in sum $ map (walk2 graph path' smallVisitedTwice) ns

e2 graph = walk2 graph (Path []) False (Cave "start")

main :: IO ()
main =
  do
    contents <- getContents

    let input =
          contents
            $> lines
            .> map (pack .> split (== '-') .> map unpack .> toPair)
            .> concatMap (\p -> [p, flipPair p])

    let graph = Graph $ Map.fromList $ groupCavePairs input

    let result1 = e1 graph
    result1 $> length .> print
    let result2 = e2 graph
    result2 $> print
