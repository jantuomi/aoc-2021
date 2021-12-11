module Main where

import Control.Monad (foldM)
import Data.Char (digitToInt)
import Data.Either (fromLeft)
import Data.List (foldl', intercalate)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Debug.Trace (trace, traceShow)
import Utils

type Dim = (Int, Int)

type Point = (Int, Int)

type State = Map Point Int

printState :: Dim -> State -> IO ()
printState dim@(xsize, ysize) state =
  let coords = [(x, y) | y <- [0 .. ysize - 1], x <- [0 .. xsize -1]]
      elems = coords $> map (state !)
   in splitEvery xsize (elems $> map show .> intercalate "")
        $> map putStrLn
        .> sequence_

pointSum :: Point -> Point -> Point
pointSum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

incAllBy1 :: State -> State
incAllBy1 = Map.map (+ 1)

flash :: State -> Dim -> Point -> State
flash state dim@(xsize, ysize) p =
  let flashed = neighbors state dim p
   in Map.mapWithKey (\p' v -> if p' `elem` flashed then v + 1 else v) state

findOverNine :: State -> [Point] -> Maybe Point
findOverNine state alreadyFlashed =
  let ps = Map.filterWithKey (\k v -> v > 9 && k `notElem` alreadyFlashed) state $> Map.keys
   in if not (null ps)
        then Just (head ps)
        else Nothing

resetFlashed :: State -> State
resetFlashed = Map.map (\v -> if v > 9 then 0 else v)

neighbors :: State -> Dim -> Point -> [Point]
neighbors state (xsize, ysize) p@(x, y) =
  let diffs = [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]
      ps = diffs $> map (pointSum p)
   in filter (`Map.member` state) ps

flashIfNotYetFlashed :: State -> Dim -> [Point] -> Point -> ([Point], State)
flashIfNotYetFlashed state dim alreadyFlashed p
  | p `elem` alreadyFlashed = (alreadyFlashed, state)
  | otherwise =
    let state' = flash state dim p
     in (p : alreadyFlashed, state')

iterateFlashes state dim alreadyFlashed =
  let unstableM = findOverNine state alreadyFlashed
   in case unstableM of
        Nothing -> (length alreadyFlashed, state)
        Just p ->
          let (alreadyFlashed', state') = flashIfNotYetFlashed state dim alreadyFlashed p
              (n, res) = iterateFlashes state' dim alreadyFlashed'
           in if length alreadyFlashed' == length alreadyFlashed
                then (length alreadyFlashed, state')
                else (n, res)

step state dim =
  let state' = incAllBy1 state
      (flashesN, state'') = iterateFlashes state' dim []
   in (flashesN, resetFlashed state'')

e1 :: State -> Dim -> (Int, State)
e1 hmap dim =
  foldl'
    ( \(n, state) _ ->
        let (n', state') = step state dim
         in (n + n', state')
    )
    (0, hmap)
    [1 .. 100]

e2 :: State -> Dim -> Int
e2 hmap dim@(xsize, ysize) =
  foldM
    ( \state s ->
        let (n', state') = step state dim
         in if n' == xsize * ysize
              then Left s
              else Right state'
    )
    hmap
    [1 ..]
    $> (\(Left s) -> s)

main :: IO ()
main =
  do
    contents <- getContents

    let input = contents $> lines .> map (map digitToInt)
    let dim@(ysize, xsize) = (length input, length (head input))
    let coords = [(x, y) | y <- [0 .. ysize - 1], x <- [0 .. xsize -1]]
    let hmap = Map.fromList $ zip coords (concat input)

    e1 hmap dim $> fst .> print
    e2 hmap dim $> print
