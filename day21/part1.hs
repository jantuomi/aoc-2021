module Part1 (simulate1, makeState1) where

import Debug.Trace (traceShow)
import Utils

data State = State
  { positions :: [Int],
    scores :: [Int],
    die :: Int,
    dieRolledTotal :: Int
  }
  deriving (Show)

makeState1 = State

rollDie :: State -> (Int, State)
rollDie prevState = (die prevState, prevState {die = die prevState `mod` 100 + 1})

setAtIndex :: Int -> b -> [b] -> [b]
setAtIndex i value lst = zip [0 ..] lst $> map (\(i', value') -> if i == i' then value else value')

position player state = positions state !! player

score player state = scores state !! player

move player prevState =
  let (dieRoll1, state1) = rollDie prevState
      (dieRoll2, state2) = rollDie state1
      (dieRoll3, state3) = rollDie state2
      state = state3 {dieRolledTotal = dieRolledTotal state3 + 3}

      newPos = (position player state + dieRoll1 + dieRoll2 + dieRoll3 - 1) `mod` 10 + 1
      newScore = score player state + newPos
   in state
        { positions = setAtIndex player newPos (positions state),
          scores = setAtIndex player newScore (scores state)
        }

checkWinCondition state
  | (scores state !! 0) >= 1000 = Left (1, scores state !! 1, dieRolledTotal state)
  | (scores state !! 1) >= 1000 = Left (0, scores state !! 0, dieRolledTotal state)
  | otherwise = Right state

takeTurn :: Int -> State -> Either (Int, Int, Int) State
takeTurn player state = do
  let movedState = move player state
  checkWinCondition movedState

simulate1 state player =
  let turnTaken = takeTurn player state
      nextPlayer = (player + 1) `mod` 2
   in case turnTaken of
        Right newState -> simulate1 newState nextPlayer
        Left result -> result
