-- Mostly plagiarized from https://github.com/sndels/aoc21 since 3D is hard
module Main where

import Data.Foldable (Foldable (foldl'), find)
import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace (traceShow)
import Utils

type Point = (Int, Int, Int)

data Cuboid = Cuboid Point Point

data StepOp = StepOn | StepOff

data Step = Step StepOp Cuboid

parseStep :: String -> Step
parseStep str =
  let [cmd, rangeStr] = words str
      ((x, y, z), (x', y', z')) =
        splitOn (== ',') rangeStr
          $> map (drop 2 .> splitOn (== '.') .> filter (not . null))
          .> (\[[x, x'], [y, y'], [z, z']] -> ((read x, read y, read z), (read x', read y', read z')))
   in case cmd of
        "on" -> Step StepOn (Cuboid (x, y, z) (x', y', z'))
        "off" -> Step StepOff (Cuboid (x, y, z) (x', y', z'))
        other -> error $ "invalid cmd: " ++ other

mini :: Point -> Point -> Point
mini (x0, y0, z0) (x1, y1, z1) = (min x0 x1, min y0 y1, min z0 z1)

maxi :: Point -> Point -> Point
maxi (x0, y0, z0) (x1, y1, z1) = (max x0 x1, max y0 y1, max z0 z1)

(<=!) :: Point -> Point -> Bool
(x0, y0, z0) <=! (x1, y1, z1) = x0 <= x1 && y0 <= y1 && z0 <= z1

volume :: Cuboid -> Int
volume (Cuboid (minX, minY, minZ) (maxX, maxY, maxZ)) =
  (maxX - minX + 1) * (maxY - minY + 1) * (maxZ - minZ + 1)

cuboidContains :: Cuboid -> Cuboid -> Bool
cuboidContains (Cuboid c0Min c0Max) (Cuboid c1Min c1Max) = c0Min <=! c1Min && c1Max <=! c0Max

cuboidIntersection :: Cuboid -> Cuboid -> Maybe Cuboid
cuboidIntersection c0 c1 = if minC <=! maxC then Just (Cuboid minC maxC) else Nothing
  where
    maxC = mini c0Max c1Max
    minC = maxi c0Min c1Min
    (Cuboid c0Min c0Max) = c0
    (Cuboid c1Min c1Max) = c1

intersects :: Cuboid -> Cuboid -> Bool
intersects c0 c1 = isJust $ cuboidIntersection c0 c1

diffPart c0@(Cuboid c0Min@(c0MinX, c0MinY, c0MinZ) c0Max@(c0MaxX, c0MaxY, c0MaxZ)) c1@(Cuboid c1Min@(c1MinX, c1MinY, c1MinZ) c1Max@(c1MaxX, c1MaxY, c1MaxZ))
  | c0MinX < c1MinX =
    Cuboid c0Min (c1MinX - 1, c0MaxY, c0MaxZ) :
    diffPart (Cuboid (c1MinX, c0MinY, c0MinZ) c0Max) c1
  | c0MinY < c1MinY =
    Cuboid c0Min (c0MaxX, c1MinY - 1, c0MaxZ) :
    diffPart (Cuboid (c0MinX, c1MinY, c0MinZ) c0Max) c1
  | c0MinZ < c1MinZ =
    Cuboid c0Min (c0MaxX, c0MaxY, c1MinZ - 1) :
    diffPart (Cuboid (c0MinX, c0MinY, c1MinZ) c0Max) c1
  | c0MaxX > c1MaxX =
    Cuboid (c1MaxX + 1, c0MinY, c0MinZ) c0Max :
    diffPart (Cuboid c0Min (c1MaxX, c0MaxY, c0MaxZ)) c1
  | c0MaxY > c1MaxY =
    Cuboid (c0MinX, c1MaxY + 1, c0MinZ) c0Max :
    diffPart (Cuboid c0Min (c0MaxX, c1MaxY, c0MaxZ)) c1
  | c0MaxZ > c1MaxZ =
    Cuboid (c0MinX, c0MinY, c1MaxZ + 1) c0Max :
    diffPart (Cuboid c0Min (c0MaxX, c0MaxY, c1MaxZ)) c1
  | otherwise = []

cuboidDifference :: Cuboid -> Cuboid -> [Cuboid]
cuboidDifference c0 c1 =
  if intersects c0 c1
    then
      if cuboidContains c1 c0
        then []
        else diffPart c0 c1
    else [c0]

doStep :: [Cuboid] -> Step -> [Cuboid]
doStep cuboids (Step op cuboid)
  | null cuboids = case op of
    StepOn -> [cuboid]
    StepOff -> []
  | otherwise = case op of
    StepOn -> cuboids ++ foldl' (\cs c -> concatMap (`cuboidDifference` c) cs) [cuboid] cuboids
    StepOff -> concatMap (`cuboidDifference` cuboid) cuboids

main :: IO ()
main = do
  contents <- getContents
  let rows = lines contents
  let steps = rows $> map parseStep
  let result1Cuboids =
        steps
          $> filter (\(Step _ cuboid) -> cuboidIntersection (Cuboid (-50, -50, -50) (50, 50, 50)) cuboid $> isJust)
          .> foldl' doStep []
  let result2Cuboids = foldl' doStep [] steps
  result1Cuboids $> map volume .> sum .> print
  result2Cuboids $> map volume .> sum .> print
