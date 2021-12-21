module Main where

import Debug.Trace (traceShow)
import Part1
import Part2
import Utils

main :: IO ()
main = do
  contents <- getContents
  let initialPositions@[p1Pos, p2Pos] = contents $> lines .> map (drop 28) .> map read :: [Int]
  let initialState = makeState1 initialPositions [0, 0] 1 0
  print initialState
  let (_, loserScore, dieRolledN) = simulate1 initialState 0
  print $ loserScore * dieRolledN

  let part2State = makeState2 (p1Pos, 0) (p2Pos, 0)

  -- this has a 50% chance of giving the right answer :D
  print $ simulate2 0 part2State