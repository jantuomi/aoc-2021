module Part2 (makeState2, simulate2) where

import Debug.Trace (traceShow)
import Utils

-- (position, score)
type Player = (Int, Int)

type State = (Player, Player)

makeState2 :: Player -> Player -> State
makeState2 p1 p2 = (p1, p2)

win player state = snd (pick player state) >= 21

pick :: Int -> State -> Player
pick 0 = fst
pick 1 = snd

move :: Int -> Int -> (Player, Player) -> State
move delta player state =
  let (oldPos, oldScore) = pick player state
      newPos = (oldPos + delta - 1) `mod` 10 + 1
      newScore = oldScore + newPos
      newState = updatePlayer player (newPos, newScore) state
   in newState

updatePlayer :: Int -> Player -> State -> State
updatePlayer 0 value (_, other) = (value, other)
updatePlayer 1 value (other, _) = (other, value)

die = [1, 2, 3]

possible3RollSums = [3 .. 9]

coeffs = [1, 3, 6, 7, 6, 3, 1]

simulate2 :: Int -> ((Int, Int), (Int, Int)) -> Integer
simulate2 player state
  | win 0 state = 1
  | win 1 state = 0
  | otherwise =
    let (pos, score) = pick player state
        nextStates = possible3RollSums $> map (\roll -> move roll player state)
        nextStatesWithC = zip coeffs nextStates
        nextPlayer = (player + 1) `mod` 2
        wins = nextStatesWithC $> map (\(c, nextState) -> c * simulate2 nextPlayer nextState) .> sum
     in wins
