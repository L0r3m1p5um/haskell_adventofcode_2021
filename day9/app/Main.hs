module Main where

import Data.List (sort)
import Data.Set qualified as Set
import System.Environment

main :: IO ()
main = do
  (filename : _) <- getArgs
  input <- readFile filename
  let heightMap = parseHeightMap input
  print "Part 1"
  print $ sum $ map ((+ 1) . index heightMap) $ lowPoints heightMap
  print "Part 2"
  let basinSizes = map (length . findBasin heightMap) (lowPoints heightMap)
  print $ product $ (take 3 . reverse . sort) basinSizes

parseHeightMap :: String -> HeightMap
parseHeightMap input = map (map (read . (: []))) (lines input)

type HeightMap = [[Int]]

rowCount :: HeightMap -> Int
rowCount = length

colCount :: HeightMap -> Int
colCount = length . head

index :: HeightMap -> (Int, Int) -> Int
index heightMap (x, y) = (heightMap !! x) !! y

isInsideMap :: HeightMap -> (Int, Int) -> Bool
isInsideMap heightMap (x, y) = x >= 0 && x <= rowCount heightMap - 1 && y >= 0 && y <= colCount heightMap - 1

isLowPoint :: HeightMap -> (Int, Int) -> Bool
isLowPoint heightMap (x, y) =
  all
    (\adj -> compareCoord heightMap adj height)
    [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  where
    height = index heightMap (x, y)
    compareCoord :: HeightMap -> (Int, Int) -> Int -> Bool
    compareCoord heightMap coord height = not (isInsideMap heightMap coord) || index heightMap coord > height

lowPoints :: HeightMap -> [(Int, Int)]
lowPoints heightMap = filter (isLowPoint heightMap) indices
  where
    indices = [(x, y) | x <- [0 .. rowCount heightMap - 1], y <- [0 .. colCount heightMap - 1]]

upslope :: HeightMap -> (Int, Int) -> Set.Set (Int, Int)
upslope heightMap (x, y)
  | index heightMap (x, y) == 9 = Set.fromList []
  | otherwise = Set.fromList $ filter (isUpslopeOf (x, y)) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  where
    isUpslopeOf :: (Int, Int) -> (Int, Int) -> Bool
    isUpslopeOf dest source
      | isInsideMap heightMap source && index heightMap source == 9 = False
      | otherwise = isInsideMap heightMap dest && isInsideMap heightMap source && index heightMap source > index heightMap dest

findBasin :: HeightMap -> (Int, Int) -> Set.Set (Int, Int)
findBasin heightMap coord = findBasinStep (Set.fromList [coord])
  where
    lowPts = lowPoints heightMap
    findBasinStep :: Set.Set (Int, Int) -> Set.Set (Int, Int)
    findBasinStep basinSet
      | newBasin == basinSet = newBasin
      | otherwise = Set.union newBasin (findBasinStep newBasin)
      where
        newBasin = Set.union basinSet (Set.unions $ Set.map (upslope heightMap) basinSet)