module Main where

import Control.Monad (foldM)
import Data.Bifunctor (bimap, first, second)
import Data.Char (digitToInt)
import Data.Function (on)
import qualified Data.List as L
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Debug.Trace (traceShow, traceShowId)
import Utils

convertHex '0' = "0000"
convertHex '1' = "0001"
convertHex '2' = "0010"
convertHex '3' = "0011"
convertHex '4' = "0100"
convertHex '5' = "0101"
convertHex '6' = "0110"
convertHex '7' = "0111"
convertHex '8' = "1000"
convertHex '9' = "1001"
convertHex 'A' = "1010"
convertHex 'B' = "1011"
convertHex 'C' = "1100"
convertHex 'D' = "1101"
convertHex 'E' = "1110"
convertHex 'F' = "1111"
convertHex other = error $ "invalid hex: " ++ [other]

hexToBinaryString "" = ""
hexToBinaryString (hex : rest) = convertHex hex ++ hexToBinaryString rest

-- | Convert binary (list of zeros and ones) to decimal
binaryToInt :: String -> Int
binaryToInt = reverse .> binaryToInt'

binaryToInt' [] = 0
binaryToInt' (x : xs) = digitToInt x + 2 * binaryToInt' xs

applyOperator opTypeId subValues = case opTypeId of
  0 -> sum subValues
  1 -> product subValues
  2 -> minimum subValues
  3 -> maximum subValues
  5 ->
    let [a, b] = subValues
     in if a > b then 1 else 0
  6 ->
    let [a, b] = subValues
     in if a < b then 1 else 0
  7 ->
    let [a, b] = subValues
     in if a == b then 1 else 0
  other -> error $ "invalid operator type id " ++ show opTypeId

parseTypeOperator :: Int -> String -> (Int, Int, String)
parseTypeOperator opTypeId binary =
  let (lengthTypeID, binary') = splitAt 1 binary $> first binaryToInt
      (versionSum, subValues, rest) = case lengthTypeID of
        0 -> parseOpLengthId0 binary'
        1 -> parseOpLengthId1 binary'
        other -> error $ "invalid length type id " ++ show other
      computedResult = applyOperator opTypeId subValues
   in (versionSum, computedResult, rest)

parseOpLengthId0 :: String -> (Int, [Int], String)
parseOpLengthId0 binary =
  let (subPacketsLength, binary') = splitAt 15 binary $> first binaryToInt
      (subPackets, binary'') = splitAt subPacketsLength binary'
      iter = iterate (\(_, _, sub) -> parse sub) (minBound, minBound, subPackets) $> tail .> takeUntil (\(_, _, rest) -> (not . null) rest)
      (subVersions, results, rests) = iter $> unzip3
   in (sum subVersions, results, binary'')

parseOpLengthId1 :: String -> (Int, [Int], String)
parseOpLengthId1 binary =
  let (subPacketsCount, binary') = splitAt 11 binary $> first binaryToInt
      iter = iterate (\(_, _, sub) -> parse sub) (minBound, minBound, binary') $> tail .> take subPacketsCount
      (subVersions, results, rests) = iter $> unzip3
   in (sum subVersions, results, last rests)

parseType4 :: [Char] -> (Int, String)
parseType4 binary =
  let (result, rest) = parseType4' binary
   in (binaryToInt result, rest)

parseType4' :: String -> (String, String)
parseType4' ('0' : rest) = splitAt 4 rest
parseType4' ('1' : rest) =
  let (binary, rest') = splitAt 4 rest
      (result, rest'') = parseType4' rest'
   in (binary ++ result, rest'')
parseType4' other = error $ "invalid type 4: " ++ other

parse :: String -> (Int, Int, String)
parse "" = (0, minBound, "")
parse binary =
  let (packetVersion, binary') = splitAt 3 binary $> first binaryToInt
      (packetTypeID, binary'') = splitAt 3 binary' $> first binaryToInt
   in case packetTypeID of
        4 ->
          let (result, rest) = parseType4 binary''
           in (packetVersion, result, rest)
        opTypeId ->
          let (typeOpVersionSum, result, rest) = parseTypeOperator opTypeId binary''
           in (packetVersion + typeOpVersionSum, result, rest)

main :: IO ()
main =
  do
    contents <- getContents

    let binary = contents $> T.pack .> T.strip .> T.unpack .> hexToBinaryString

    parse binary $> print
