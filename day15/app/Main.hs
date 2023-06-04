{-# LANGUAGE TupleSections #-}

module Main where

import Data.List (findIndex)
import Data.Maybe (fromJust)
import Part2
import System.Environment (getArgs)

main :: IO ()
main = do
  (filename : _) <- getArgs
  input <- readFile filename
  -- putStrLn "Part 1"
  -- let grid = parseGrid input
  -- shortestPath <- findShortestPath grid
  -- print $ (snd . head) shortestPath
  solvePart2 input

type Grid = [[Int]]

parseGrid :: String -> Grid
parseGrid input = map (map (\x -> read [x])) $ lines input

atCoord :: Grid -> (Int, Int) -> Int
atCoord grid (x, y) = ((!! x) . (!! y)) grid

type Path = [((Int, Int), Int)]

scorePath :: Grid -> [(Int, Int)] -> Path
scorePath grid = foldr buildScore []
  where
    buildScore :: (Int, Int) -> Path -> Path
    buildScore coord [] = [(coord, 0)]
    buildScore coord acc = (coord, atCoord grid coord + (snd . head) acc) : acc

endsAt :: (Int, Int) -> Path -> Bool
endsAt coord path = (fst . head) path == coord

findShortestPath :: Grid -> IO Path
findShortestPath grid = do
  print "testing"
  endPaths <- findEndPaths [[((0, 0), 0)]]
  return (head endPaths)
  where
    nextPath' = nextPath grid
    minimizeRisk' = minimizeRisk grid
    isFinal = endsAt (length (head grid) - 1, length grid - 1)
    trimLargePaths :: [Path] -> [Path]
    trimLargePaths paths = filter (\x -> (snd . head) x <= maxPathSize) paths
      where
        maxX = length (head grid) - 1
        maxY = length grid - 1
        basicPath = scorePath grid $ map (,0) [0 .. maxX] ++ map (maxX,) [0 .. maxY]
        maxPathSize
          | not (any isFinal paths) = (snd . head) basicPath
          | otherwise = minimum $ map (snd . head) (filter isFinal paths)

    findEndPaths :: [Path] -> IO [Path]
    findEndPaths paths
      | all isFinal paths = return $ minimizeRisk' paths
      | otherwise = do
          let next = concatMap nextPath' paths
          print $ "Next paths " ++ show (length next)
          let minimized = minimizeRisk' next
          print $ "Minimized " ++ show (length minimized)
          let trimmed = trimLargePaths minimized
          print $ "trimmed " ++ show (length trimmed)
          findEndPaths trimmed

nextPath :: Grid -> Path -> [Path]
nextPath grid path
  | endsAt endPoint path = [path]
  | otherwise = map (\x -> (x, score + atCoord grid x) : path) nextCoord
  where
    endPoint = (length (head grid) - 1, length grid - 1)
    ((lastX, lastY), score) = head path
    nextCoord = filter isValid [(lastX + 1, lastY), (lastX - 1, lastY), (lastX, lastY + 1), (lastX, lastY - 1)]
    isValid :: (Int, Int) -> Bool
    isValid coord = isInGrid coord && coord `notElem` map fst path
    isInGrid :: (Int, Int) -> Bool
    isInGrid (x, y) = x >= 0 && y >= 0 && x < length (head grid) && y < length grid

minimizeRisk :: Grid -> [Path] -> [Path]
minimizeRisk grid = foldr (flip (addMinimizedRiskPath grid)) []

addMinimizedRiskPath :: Grid -> [Path] -> Path -> [Path]
addMinimizedRiskPath grid paths newPath
  | null intersectingPaths = newPath : paths
  | (snd . head) newPath >= minimumScore = paths
  | (snd . head) newPath < minimumScore = nonIntersectingPaths ++ map (replaceSubpath newPath) intersectingPaths
  where
    endCoord = (fst . head) newPath
    intersectingPaths = filter (\path -> (fst . head) newPath `elem` map fst path) paths
    nonIntersectingPaths = filter (\path -> (fst . head) newPath `notElem` map fst path) paths
    matchingSubpaths = map (dropWhile (\x -> fst x /= endCoord)) intersectingPaths
    minimumScore = minimum $ map (snd . head) matchingSubpaths
    replaceSubpath :: Path -> Path -> Path
    replaceSubpath subpath path = scorePath grid (pathHead ++ pathTail)
      where
        pathHead = takeWhile (/= (fst . head) subpath) $ map fst path
        pathTail = map fst subpath
