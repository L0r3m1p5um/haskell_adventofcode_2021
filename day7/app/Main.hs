{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List.Split (splitOn)
import System.Environment (getArgs)

main :: IO ()
main = do
  (filename : _) <- getArgs
  input <- readFile filename
  let startingPos :: [Int] = map read $ splitOn "," input
  print "Part 1"
  print $ minFuelSpent startingPos
  print "Part 2"
  print $ minFuelSpent' startingPos

fuelSpent :: [Int] -> Int -> Int
fuelSpent [] _ = 0
fuelSpent (x : xs) pos = abs (pos - x) + fuelSpent xs pos

minFuelSpent :: [Int] -> Int
minFuelSpent pos = minimum $ map (fuelSpent pos) [0 .. length pos]

fuelSpent' :: [Int] -> Int -> Int
fuelSpent' [] _ = 0
fuelSpent' (x : xs) pos = sum [0 .. abs (pos - x)] + fuelSpent' xs pos

minFuelSpent' :: [Int] -> Int
minFuelSpent' pos = minimum $ map (fuelSpent' pos) [0 .. length pos]
