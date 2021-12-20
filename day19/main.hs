{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad (join)
import Data.Foldable (find)
import Data.List (deleteBy, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Debug.Trace (trace, traceShow)
import Utils

type Point = (Int, Int, Int)

type Scanner = (Int, [Point])

cmap :: t -> [t -> b] -> [b]
cmap x = map ($ x)

dot (a, b, c) (d, e, f) = a * d + b * e + c * f

mmulti (a, b, c) v = (dot a v, dot b v, dot c v)

transforms v =
  let x = mmulti ((1, 0, 0), (0, 0, -1), (0, 1, 0))
      y = mmulti ((0, 0, 1), (0, 1, 0), (-1, 0, 0))
      z = mmulti ((0, -1, 0), (1, 0, 0), (0, 0, 1))
   in cmap
        v
        [ id,
          x,
          y,
          z,
          x . x,
          x . y,
          x . z,
          y . x,
          y . y,
          z . y,
          z . z,
          x . x . x,
          x . x . y,
          x . x . z,
          x . y . x,
          x . y . y,
          x . z . z,
          y . x . x,
          y . y . y,
          z . z . z,
          x . x . x . y,
          x . x . y . x,
          x . y . x . x,
          x . y . y . y
        ]

transformBeacons :: [Point] -> [[Point]]
transformBeacons beacons =
  beacons $> map transforms .> transpose

checkAssumedMatch beacons1 beacons2 (b1, b2) =
  let delta = calculateDelta b1 b2
      translatedBeacons2 = translateBeacons delta beacons2
      matchingCommonBeacons = translatedBeacons2 $> filter (`elem` beacons1)
   in if length matchingCommonBeacons >= 12
        then Just (translatedBeacons2, delta)
        else Nothing

checkCompositeAndScannerTf :: Scanner -> Scanner -> Maybe (Int, [Point], Point)
checkCompositeAndScannerTf (s1, beacons1) (s2, beacons2) =
  let pairings = [(b1, b2) | b1 <- beacons1, b2 <- beacons2]
      found = map (checkAssumedMatch beacons1 beacons2) pairings $> find isJust .> join
      -- if found
      composite = concat .> nub
   in case found of
        Just (newBeacons2, newScannerPos) -> Just (s2, composite [beacons1, newBeacons2], newScannerPos)
        Nothing -> Nothing

manhattanD (x, y, z) (x', y', z') = abs (x - x') + abs (y - y') + abs (z - z')

calculateDelta :: Point -> Point -> Point
calculateDelta (x, y, z) (x', y', z') = (x - x', y - y', z - z')

translateBeacons :: Point -> [Point] -> [Point]
translateBeacons (dx, dy, dz) =
  map (\(x, y, z) -> (x + dx, y + dy, z + dz))

check2CompositeAndScanner :: Scanner -> Scanner -> Maybe (Int, [Point], Point)
check2CompositeAndScanner scanner1@(s1, beacons1) scanner2@(s2, beacons2) =
  let beacons2Tfs = transformBeacons beacons2
      found = beacons2Tfs $> map (\bs -> checkCompositeAndScannerTf scanner1 (s2, bs)) .> find isJust .> join
   in found

compute _ [] = error "empty scanners list"
compute scannerPositions [composite] = Just (composite, scannerPositions)
compute scannerPositions (scanner@(s, _) : others) =
  let scannerPairs = others $> map (scanner,)
      found = scannerPairs $> map (uncurry check2CompositeAndScanner) .> find isJust .> join
   in case found of
        Just (droppedS, compositeBeacons, newScannerPos) ->
          let newOthers = filter (fst .> (/=) droppedS) others
              newComposite = (s, compositeBeacons)
              newScannerPositions = newScannerPos : scannerPositions
           in compute newScannerPositions (newComposite : newOthers)
        Nothing -> compute scannerPositions others

main :: IO ()
main = do
  contents <- getContents
  let scanners =
        contents
          $> lines
          .> splitOn null
          .> map (drop 1)
          .> map (map (splitOn (== ',') .> map read .> \[x, y, z] -> (x, y, z)))
          .> zip [0 ..]

  let Just ((_, resultBeacons), resultScannerPositions) = compute [] scanners
  length resultBeacons $> print
  let scannerPairs = [(s1, s2) | s1 <- resultScannerPositions, s2 <- resultScannerPositions, s1 /= s2]
  scannerPairs $> map (uncurry manhattanD) .> maximum .> print
