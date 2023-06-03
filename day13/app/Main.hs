module Main where

import Data.Bifunctor (first)
import Data.List.Split (splitOn)
import Data.Maybe as Maybe
import Data.Set qualified as Set
import Data.Tuple (swap)
import System.Environment (getArgs)

main :: IO ()
main = do
  (filename : _) <- getArgs
  input <- readFile filename
  let (points, rest) = first Set.fromList $ parseLines (lines input) parsePoint
      (instructions, _) = parseLines (drop 1 rest) parseInstruction
  putStrLn "Part 1"
  print $ length $ Set.map (foldOn $ head instructions) points
  putStrLn "Part 2"
  printGrid $ pointsToGrid $ foldl (\acc x -> Set.map (foldOn x) acc) points instructions

parseLines :: [String] -> (String -> Maybe a) -> ([a], [String])
parseLines input parseLine = (parsed, drop (length parsed) input)
  where
    parsed = catMaybes $ takeWhile isJust (map parseLine input)

parsePoint :: String -> Maybe (Int, Int)
parsePoint line
  | length splitLine == 2 = Just ((read . head) splitLine, read (splitLine !! 1))
  | otherwise = Nothing
  where
    splitLine = splitOn "," line

data Instruction = X Int | Y Int deriving (Show, Eq, Ord)

parseInstruction :: String -> Maybe Instruction
parseInstruction line
  | instruction == "fold along y" && length splitLine == 2 = Just $ Y (read $ splitLine !! 1)
  | instruction == "fold along x" && length splitLine == 2 = Just $ X (read $ splitLine !! 1)
  | otherwise = Nothing
  where
    splitLine = splitOn "=" line
    instruction = head splitLine

foldOnY :: Int -> (Int, Int) -> (Int, Int)
foldOnY line (x, y) = (x, -(abs (y - line) - line))

foldOn :: Instruction -> (Int, Int) -> (Int, Int)
foldOn (Y line) point = foldOnY line point
foldOn (X line) point = swap $ foldOnY line $ swap point

pointsToGrid :: Set.Set (Int, Int) -> [[Bool]]
pointsToGrid points = map makeLine (take (maxY + 1) [0 ..])
  where
    maxX = maximum $ Set.map fst points
    maxY = maximum $ Set.map snd points
    makeLine :: Int -> [Bool]
    makeLine index = map (\x -> (x, index) `elem` pointsInLine) (take (maxX + 1) [0 ..])
      where
        pointsInLine = Set.filter (\(_, y) -> y == index) points

printGrid :: [[Bool]] -> IO ()
printGrid grid = do
  let toString :: [Bool] -> String
      toString = map charValue
        where
          charValue :: Bool -> Char
          charValue False = '.'
          charValue True = '#'
  mapM_ (putStrLn . toString) grid