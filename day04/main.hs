module Main where

import Data.Foldable (find)
import Data.Maybe (fromJust, isJust)
import Debug.Trace (trace, traceShow)
import Utils

type Board = [[Int]]

toGroupsOfN :: Int -> [a] -> [[a]]
toGroupsOfN n [] = []
toGroupsOfN n lst = take n lst : toGroupsOfN n (drop n lst)

rowBingo :: [Int] -> Board -> Bool
rowBingo draws = any (all (`elem` draws))

findBingo :: [Int] -> Board -> Maybe Int
findBingo draws board =
  let horizFound = rowBingo draws board
      vertFound = rowBingo draws (transpose board)
   in if horizFound || vertFound
        then Just $ sumUnmarked draws board
        else Nothing

sumUnmarked :: [Int] -> Board -> Int
sumUnmarked draws board =
  let sumRowUnmarked row = filter (`notElem` draws) row $> sum
   in board $> map sumRowUnmarked .> sum

recur :: [Int] -> Int -> [Board] -> ()
recur draws n boards
  | n > length draws = ()
  | otherwise =
    let drawn = take n draws
        notDrawn = drop n draws
        bingoResults = boards $> mapWithIndex (findBingo drawn) .> filter (\(_, m) -> isJust m)
        winningBoards = map (\(i, m) -> (i, last drawn * fromJust m)) bingoResults
        nWinning = length winningBoards
        indicesToRemove = map fst winningBoards
        winningScores = map snd winningBoards
        restBoards = removeAt indicesToRemove boards
     in trace ("Found " ++ show nWinning ++ " results at " ++ show n ++ ": " ++ show winningScores ++ ", boards left: " ++ show (length restBoards)) $
          recur draws (n + 1) restBoards

run :: [Int] -> [Board] -> ()
run draws = recur draws 1

main :: IO ()
main =
  do
    contents <- getContents
    let input =
          contents
            $> lines
            .> filter (not . null)

    let draws = head input $> splitOn (== ',') .> map read :: [Int]
    let boards = tail input $> map (words .> map read) .> toGroupsOfN 5 :: [Board]
    run draws boards $> print
