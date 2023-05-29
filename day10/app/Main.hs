module Main where

import Control.Monad
import Control.Monad.Trans.State
import Data.List (find, sort)
import System.Environment (getArgs)

main :: IO ()
main = do
  (filename : _) <- getArgs
  input <- readFile filename
  print "Part 1"
  let testSyntax = map parseSyntax "(>"
      syntax = map (map parseSyntax) (lines input)
      checked = map (\x -> runState (checkSyntax x) []) syntax
      results = map fst checked
      stacks = map snd (filter (\x -> fst x == Ok) checked)
  print $ sum $ map score results
  print "Part 2"
  print $ middleScore $ map scoreAutocomplete stacks

data Syntax = Open Bracket | Close Bracket deriving (Show, Eq)

data Bracket = Paren | Square | Curly | Angle deriving (Show, Eq)

data Result = Ok | Corrupt Bracket deriving (Show, Eq)

parseSyntax :: Char -> Syntax
parseSyntax '(' = Open Paren
parseSyntax ')' = Close Paren
parseSyntax '[' = Open Square
parseSyntax ']' = Close Square
parseSyntax '{' = Open Curly
parseSyntax '}' = Close Curly
parseSyntax '<' = Open Angle
parseSyntax '>' = Close Angle

match :: Syntax -> Syntax -> Result
match (Open opening) (Close closing) = if opening == closing then Ok else Corrupt closing
match _ (Close closing) = Corrupt closing

score :: Result -> Int
score (Corrupt Paren) = 3
score (Corrupt Square) = 57
score (Corrupt Curly) = 1197
score (Corrupt Angle) = 25137
score _ = 0

scoreAutocomplete :: [Syntax] -> Int
scoreAutocomplete syntax = foldl (\acc x -> 5 * acc + x) 0 bracketScores
  where
    bracketScores = map (scoreBracket . getBracket) syntax
    getBracket :: Syntax -> Bracket
    getBracket (Open x) = x
    getBracket (Close x) = x
    scoreBracket :: Bracket -> Int
    scoreBracket Paren = 1
    scoreBracket Square = 2
    scoreBracket Curly = 3
    scoreBracket Angle = 4

middleScore :: [Int] -> Int
middleScore scores = sorted !! len
  where
    sorted = sort scores
    len = length scores `div` 2

type Stack = [Syntax]

push :: Syntax -> State Stack ()
push x = state $ \xs -> ((), x : xs)

pop :: State Stack Syntax
pop = state $ \(x : xs) -> (x, xs)

checkSyntax :: [Syntax] -> State Stack Result
checkSyntax input = do
  results <- mapM checkNext input
  let result = find (/= Ok) results
  case result of
    Nothing -> return Ok
    Just corrupt -> return corrupt

checkNext :: Syntax -> State Stack Result
checkNext (Open x) = do
  push (Open x)
  return Ok
checkNext (Close x) = do
  bracket <- pop
  return $ match bracket (Close x)
