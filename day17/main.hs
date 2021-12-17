module Main where

import Data.Function (on)
import qualified Data.List as L
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Debug.Trace (traceShow, traceShowId)
import Utils

type Point = (Int, Int)

data State = State
  { px :: Int,
    py :: Int,
    vx :: Int,
    vy :: Int
  }
  deriving (Show)

-- | Returns the opposite corner points of the target area rectangle
parseTargetArea :: String -> (Point, Point)
parseTargetArea input =
  drop 15 input $> T.pack
    .> T.replace (T.pack "y=") T.empty
    .> T.replace (T.pack "..") (T.pack ",")
    .> T.replace (T.pack " ") T.empty
    .> T.splitOn (T.pack ",")
    .> map (T.unpack .> read)
    .> \[x1, x2, y1, y2] -> ((x1, y1), (x2, y2))

rightHalfPlanePoints = [(x, y) | d <- [0 ..], y <- [- d .. d], x <- [0 .. d], abs x + abs y == d]

simulateStep :: State -> State
simulateStep state =
  State
    { px = px state + vx state,
      py = py state + vy state,
      vx = max (vx state - 1) 0,
      vy = vy state - 1
    }

pointInRect :: (Point, Point) -> Point -> Bool
pointInRect ((x1, y1), (x2, y2)) (x, y) =
  let minX = min x1 x2
      maxX = max x1 x2
      minY = min y1 y2
      maxY = max y1 y2
   in x >= minX && x <= maxX && y >= minY && y <= maxY

simulateWhile :: (State -> Bool) -> Point -> [Point]
simulateWhile cond (vx0, vy0) =
  let initialState =
        State
          { px = 0,
            py = 0,
            vx = vx0,
            vy = vy0
          }
      steps = iterate simulateStep initialState
      result = takeUntil cond steps
   in map (\state -> (px state, py state)) result

run :: (Point, Point) -> (Int, Int)
run targetArea@((x1, y1), (x2, y2)) =
  let v0s = take 100000 rightHalfPlanePoints
      xBound = x2
      yBound = min 0 y1
      trajectories = v0s $> map (simulateWhile (\(State px py _ _) -> px <= xBound && py >= yBound))
      successful = trajectories $> filter (any (pointInRect targetArea))
   in (snd $ L.maximumBy (compare `on` snd) (concat successful), length successful)

main :: IO ()
main = do
  contents <- getContents
  let targetArea = parseTargetArea contents

  print $ run targetArea