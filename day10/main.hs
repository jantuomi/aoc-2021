module Main where

import Data.Either (fromLeft, fromRight)
import Data.List (foldl', sort)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Utils

data Expr
  = Paren [Expr]
  | Bracket [Expr]
  | Curly [Expr]
  | Angle [Expr]
  deriving (Show)

data Failure
  = Incomplete
  | Corrupted Char
  deriving (Show)

type Computation a = Either Failure a

consume expected (c : cs) =
  if c /= expected
    then Left (Corrupted c)
    else Right cs
consume expected [] = Left Incomplete

pairs =
  Map.fromList
    [ ('(', ')'),
      ('[', ']'),
      ('{', '}'),
      ('<', '>')
    ]

opening = Map.keys pairs

closing = Map.elems pairs

expr :: Char -> [Expr] -> Expr
expr '(' children = Paren children
expr '[' children = Bracket children
expr '{' children = Curly children
expr '<' children = Angle children
expr c _ = error $ "invalid c: " ++ [c]

parse :: String -> Computation ([Expr], String)
parse s
  | null s = Left Incomplete
  | head s `elem` opening =
    let (c : cs) = s
        closing = pairs ! c
     in do
          (children, rest) <- parse cs
          rest' <- consume closing rest
          (nextExprs, rest'') <-
            if not (null rest')
              then parse rest'
              else Right ([], rest')
          Right (expr c children : nextExprs, rest'')
  | head s `elem` closing = Right ([], s)
  | otherwise = error "unreachable"

isCorrupted expr = case expr of
  Left (Corrupted c) -> True
  _ -> False

fromCorrupted expr = case expr of
  Left (Corrupted c) -> c
  a -> error $ "not corrupted: " ++ show a

score1 ')' = 3
score1 ']' = 57
score1 '}' = 1197
score1 '>' = 25137
score1 a = error $ "not valid for scoring: " ++ [a]

e1 rows =
  let parsed = map parse rows
      corrupted = parsed $> filter isCorrupted .> map fromCorrupted
      scores = corrupted $> map score1
   in sum scores

repair :: [Char] -> [Char] -> Either Failure [Char]
repair expectStack s
  | null s = Right expectStack
  | head s `elem` opening =
    let (c : cs) = s
        correspondingClosing = pairs ! c
        expectStack' = correspondingClosing : expectStack
     in repair expectStack' cs
  | head s `elem` closing =
    do
      let (c : cs) = s
      let (ec : ecs) = expectStack
      if c /= ec
        then Left (Corrupted c)
        else repair ecs cs
  | otherwise = error "unreachable"

score2 ')' = 1
score2 ']' = 2
score2 '}' = 3
score2 '>' = 4
score2 a = error $ "not valid for scoring: " ++ [a]

fixToScore = foldl' (\acc c -> 5 * acc + score2 c) 0

e2 rows =
  let parsed = map (repair []) rows
      fixesM = parsed $> filter (not . isCorrupted)
      fixes = fixesM $> map (\(Right fix) -> fix)
      scores = fixes $> map fixToScore
      scoresSorted = sort scores
      scoresN = length scoresSorted
   in scoresSorted !! (scoresN `div` 2)

main :: IO ()
main =
  do
    contents <- getContents

    let input = contents $> lines

    e1 input $> print
    e2 input $> print
