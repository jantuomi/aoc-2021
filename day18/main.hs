module Main where

import Data.Char (isDigit)
import Data.Foldable (Foldable (foldl'))
import Data.List (foldl1')
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Debug.Trace (trace, traceShow, traceShowId)
import Utils

-- Snailfish number element
data SFFlatE = SFFlatRegular Int | SFFlatStart | SFFlatEnd | SFFlatSep deriving (Eq)

data SFTreeE = SFTreeRegular Int | SFTreePair (SFTreeE, SFTreeE) deriving (Eq)

-- Snailfish number
newtype SFFlatN = SFFlatN [SFFlatE] deriving (Eq)

newtype SFTreeN = SFTreeN SFTreeE deriving (Eq)

instance Num SFTreeN where
  a + b = sfnSum a b
  abs (SFTreeN a) = SFTreeN $ SFTreeRegular $ sfeMagn a

instance Show SFTreeN where
  show (SFTreeN sfe) = show sfe

instance Show SFTreeE where
  show (SFTreeRegular int) = show int
  show (SFTreePair (a, b)) = "[" ++ show a ++ "," ++ show b ++ "]"

instance Show SFFlatN where
  show (SFFlatN sfes) = concatMap showSFFlatE sfes

instance Show SFFlatE where
  show = showSFFlatE

showSFFlatE (SFFlatRegular int) = show int
showSFFlatE SFFlatStart = "["
showSFFlatE SFFlatEnd = "]"
showSFFlatE SFFlatSep = ","

expectSFFlatE :: SFFlatE -> [SFFlatE] -> [SFFlatE]
expectSFFlatE expectedE (e : rest) =
  if expectedE == e
    then rest
    else error $ "unexpected flat element: " ++ show e ++ ", expected: " ++ show expectedE
expectSFFlatE _ [] = error "expectSFFlatE: empty []"

flatToTree' :: [SFFlatE] -> (SFTreeE, [SFFlatE])
flatToTree' (SFFlatStart : rest) =
  let (result1, rest1) = flatToTree' rest
      rest2 = expectSFFlatE SFFlatSep rest1
      (result2, rest3) = flatToTree' rest2
      rest4 = expectSFFlatE SFFlatEnd rest3
   in (SFTreePair (result1, result2), rest4)
flatToTree' ((SFFlatRegular int) : rest) =
  (SFTreeRegular int, rest)
flatToTree' other = error $ "weird flatToTree: " ++ show (SFFlatN other)

flatToTree :: SFFlatN -> SFTreeN
flatToTree (SFFlatN sfes) =
  let (result, rest) = flatToTree' sfes
   in if null rest
        then SFTreeN result
        else error $ "flatToTree incomplete: " ++ show (SFFlatN rest)

treeToFlat' :: SFTreeE -> [SFFlatE]
treeToFlat' (SFTreeRegular int) = [SFFlatRegular int]
treeToFlat' (SFTreePair (a, b)) = concat [[SFFlatStart], treeToFlat' a, [SFFlatSep], treeToFlat' b, [SFFlatEnd]]

treeToFlat :: SFTreeN -> SFFlatN
treeToFlat (SFTreeN sfe) = SFFlatN $ treeToFlat' sfe

sfnSum :: SFTreeN -> SFTreeN -> SFTreeN
sfnSum (SFTreeN a) (SFTreeN b) =
  let sum = SFTreeN $ SFTreePair (a, b)
   in sfnReduce $ sum

sfnReduceM :: SFTreeN -> Either SFTreeN SFTreeN
sfnReduceM sfn =
  let explodeM tree =
        let exploded = sfnExplode tree
         in if exploded /= tree then Left exploded else Right exploded
      splitM tree =
        let splitted = sfnSplit tree
         in if splitted /= tree then Left splitted else Right splitted
   in do
        result1 <- explodeM sfn
        result2 <- splitM sfn
        return sfn

sfnReduce sfn =
  let afterReduce = sfnReduceM sfn
   in case afterReduce of
        Left sfn' -> sfnReduce sfn'
        Right sfn' -> sfn'

sfeMagn :: SFTreeE -> Int
sfeMagn (SFTreeRegular int) = int
sfeMagn (SFTreePair (a, b)) = 3 * sfeMagn a + 2 * sfeMagn b

sfeSplitM :: SFTreeE -> Either SFTreeE SFTreeE
sfeSplitM sfe@(SFTreeRegular int)
  | int >= 10 = Left $ SFTreePair (SFTreeRegular $ int `div` 2, SFTreeRegular $ int - int `div` 2)
  | otherwise = Right sfe
sfeSplitM (SFTreePair (a, b)) = do
  lhs <- case sfeSplitM a of
    Left a' -> Left $ SFTreePair (a', b)
    Right a' -> Right a'
  rhs <- case sfeSplitM b of
    Left b' -> Left $ SFTreePair (a, b')
    Right b' -> Right b'
  Right $ SFTreePair (lhs, rhs)

sfnSplit :: SFTreeN -> SFTreeN
sfnSplit (SFTreeN sfe) = sfe $> sfeSplitM .> fromSymEither .> SFTreeN

sfnExplodeAtDepth :: Int -> Int -> SFFlatN -> Maybe ([SFFlatE], Int, SFFlatE, SFFlatE)
sfnExplodeAtDepth idx 0 (SFFlatN (SFFlatStart : rest1)) =
  let (leftValue : rest2) = rest1
      (SFFlatSep : rest3) = rest2
      (rightValue : rest4) = rest3
      (SFFlatEnd : rest5) = rest4
   in Just (SFFlatRegular 0 : rest5, idx, leftValue, rightValue)
sfnExplodeAtDepth idx depth (SFFlatN (sfe : rest)) =
  let depth' = case sfe of
        SFFlatStart -> depth - 1
        SFFlatEnd -> depth + 1
        _ -> depth
   in sfnExplodeAtDepth (idx + 1) depth' (SFFlatN rest) $> fmap (\(rest, b, c, d) -> (sfe : rest, b, c, d))
sfnExplodeAtDepth idx depth (SFFlatN []) = Nothing

replaceLeftRegularIfExists :: Int -> SFFlatE -> SFFlatN -> SFFlatN
replaceLeftRegularIfExists idx replacement sfn@(SFFlatN sfes) =
  let elemAtIdx = sfes !! idx
      (SFFlatRegular rInt) = replacement
   in case elemAtIdx of
        SFFlatRegular int -> SFFlatN $ take idx sfes ++ [SFFlatRegular (rInt + int)] ++ drop (idx + 1) sfes
        other -> if idx > 0 then replaceLeftRegularIfExists (idx - 1) replacement sfn else sfn

replaceRightRegularIfExists :: Int -> SFFlatE -> SFFlatN -> SFFlatN
replaceRightRegularIfExists idx replacement sfn@(SFFlatN sfes) =
  let elemAtIdx = sfes !! idx
      (SFFlatRegular rInt) = replacement
   in case elemAtIdx of
        SFFlatRegular int -> SFFlatN $ take idx sfes ++ [SFFlatRegular (rInt + int)] ++ drop (idx + 1) sfes
        other -> if idx < length sfes - 1 then replaceRightRegularIfExists (idx + 1) replacement sfn else sfn

sfnExplodeM :: SFTreeN -> Maybe SFTreeN
sfnExplodeM tree = do
  let flat1 = treeToFlat tree
  (flat2', idx, leftValue, rightValue) <- sfnExplodeAtDepth 0 4 flat1
  let flat2 = SFFlatN flat2'
  let flat3 = replaceLeftRegularIfExists (idx - 1) leftValue flat2
  let flat4 = replaceRightRegularIfExists (idx + 1) rightValue flat3
  return (flatToTree flat4)

sfnExplode :: SFTreeN -> SFTreeN
sfnExplode tree = fromMaybe tree $ sfnExplodeM tree

parseStringToSFFlatN ('[' : rest) = SFFlatStart : parseStringToSFFlatN rest
parseStringToSFFlatN (']' : rest) = SFFlatEnd : parseStringToSFFlatN rest
parseStringToSFFlatN (',' : rest) = SFFlatSep : parseStringToSFFlatN rest
parseStringToSFFlatN (' ' : rest) = parseStringToSFFlatN rest
parseStringToSFFlatN "" = []
parseStringToSFFlatN other = SFFlatRegular (read $ takeWhile isDigit other) : parseStringToSFFlatN (dropWhile isDigit other)

main :: IO ()
main = do
  contents <- getContents
  let flats = contents $> lines .> map (parseStringToSFFlatN .> SFFlatN)
  let trees = flats $> map flatToTree
  let reduceResult = foldl1' (+) trees
  print reduceResult
  let magnResult = abs reduceResult
  print magnResult

  let allPairs = [(a, b) | a <- trees, b <- trees, a /= b]
  let biggestMagn = allPairs $> map (uncurry (+) .> abs .> (\(SFTreeN (SFTreeRegular mag)) -> mag)) .> maximum
  print biggestMagn
