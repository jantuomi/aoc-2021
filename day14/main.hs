{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Function (on)
import qualified Data.List as L
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.Text (pack, replace, unpack)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Debug.Trace (traceShow)
import Utils

recur1 :: Vector Char -> Map (Char, Char) Char -> Int -> Vector Char
recur1 initial rules n
  | n == 0 = initial
  | otherwise =
    let pairs = V.zip initial (V.tail initial)
        inserted = pairs $> V.concatMap (\p@(a, _) -> V.fromList [a, rules ! p])
        result = V.snoc inserted (V.last initial)
     in recur1 result rules (n - 1)

e1 initial rules =
  let polymer = recur1 (V.fromList initial) rules 10
      lengthPairs = polymer $> V.map (\c -> (c, V.length $ V.filter (== c) polymer))
      (mc, mcN) = lengthPairs $> V.maximumBy (compare `on` snd)
      (lc, lcN) = lengthPairs $> V.minimumBy (compare `on` snd)
   in mcN - lcN

type CPair = (Char, Char)

type PairFreqMap = Map CPair Integer

type CFreqMap = Map Char Integer

type RuleMap = Map CPair Char

type Maps = (PairFreqMap, CFreqMap, RuleMap)

recur2 :: Maps -> Int -> CFreqMap
recur2 maps@(pairFreq, cFreq, rules) n
  | n == 0 = cFreq
  | otherwise =
    let pairs = M.keys pairFreq
        (pairFreq', cFreq') =
          L.foldl'
            ( \(accPairFreq, accCFreq) pair@(a, b) ->
                let c = rules ! pair
                    totalPairFreq = pairFreq ! pair
                    pairFreq' =
                      accPairFreq
                        $> M.insertWith (+) (a, c) totalPairFreq
                        .> M.insertWith (+) (c, b) totalPairFreq
                        .> M.adjust (\v -> v - totalPairFreq) pair
                    cFreq' = M.insertWith (+) c totalPairFreq accCFreq
                 in (pairFreq', cFreq')
            )
            (pairFreq, cFreq)
            pairs
     in recur2 (pairFreq', cFreq', rules) (n - 1)

e2 initial rules =
  let pairs = zip initial (tail initial)
      pairFreqMap = L.foldl' (\acc p -> M.insertWith (+) p 1 acc) M.empty pairs
      cFreqMap = L.foldl' (\acc c -> M.insertWith (+) c 1 acc) M.empty initial
      resultCFreqMap = recur2 (pairFreqMap, cFreqMap, rules) 40
      mcN = L.maximum (M.elems resultCFreqMap)
      lcN = L.minimum (M.elems resultCFreqMap)
   in mcN - lcN

main :: IO ()
main =
  do
    contents <- getContents

    let input = contents $> lines
    let initial = head input
    let rules' =
          tail (tail input)
            $> map (pack .> replace (pack " -> ") (pack "") .> unpack)
            .> map (\[x, y, z] -> ((x, y), z))

    let rules = M.fromList rules'
    e1 initial rules $> print
    e2 initial rules $> print
