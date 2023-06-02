module Main where

import Control.Monad
import Control.Monad.Trans.State
import Data.List (findIndex)
import Data.Maybe (mapMaybe)
import System.Environment

main :: IO ()
main = do
  (filename : steps : _) <- getArgs
  input <- readFile filename
  let grid = parseGrid input
  print "Part 1"
  let result = runState (simulation $ read steps) grid
      count = fst result
      finalGrid = snd result
  print $ runState (simulation2 1) grid

type OctopusGrid = (Int, [(Int, Bool)])

simulation :: Int -> State OctopusGrid Int
simulation steps = do
  foldM (\acc x -> do val <- x; return (acc + val)) 0 (replicate steps simulationStep)

simulation2 :: Int -> State OctopusGrid Int
simulation2 count = do
  simulationStep
  (len, grid) <- get
  if all (\x -> fst x == 0) grid
    then return count
    else simulation2 (count + 1)

simulationStep :: State OctopusGrid Int
simulationStep = do
  raiseEnergy
  flash
  count <- countFlashed
  reset
  return count

printGrid :: OctopusGrid -> IO ()
printGrid (len, grid) = do
  let nums = map fst grid
      split :: [Int] -> [[Int]] -> [[Int]]
      split [] xs = xs
      split xs ys = split (drop len xs) (take len xs : ys)
      lines = reverse $ split nums []
  mapM_ print lines

coord :: OctopusGrid -> (Int, Int) -> (Int, Bool)
coord (len, grid) (x, y) = grid !! (y + len * x)

updateIndex :: Int -> (a -> a) -> [a] -> [a]
updateIndex index fun list = first ++ fun x : second
  where
    (first, x : second) = splitAt index list

adjacent :: Int -> State OctopusGrid [Int]
adjacent index = do
  (len, grid) <- get
  let left :: Int -> Maybe Int
      left x
        | notLeftEdge x = Just (x - 1)
        | otherwise = Nothing
      right x
        | notRightEdge x = Just (x + 1)
        | otherwise = Nothing
      below x
        | notBottom x = Just (x + len)
        | otherwise = Nothing
      above x
        | notTop x = Just (x - len)
        | otherwise = Nothing
      topLeft x
        | notTop x && notLeftEdge x = Just (x - len - 1)
        | otherwise = Nothing
      topRight x
        | notTop x && notRightEdge x = Just (x - len + 1)
        | otherwise = Nothing
      bottomLeft x
        | notBottom x && notLeftEdge x = Just (x + len - 1)
        | otherwise = Nothing
      bottomRight x
        | notBottom x && notRightEdge x = Just (x + len + 1)
        | otherwise = Nothing

      notLeftEdge :: Int -> Bool
      notLeftEdge x = (x - 1) `mod` len /= len - 1
      notRightEdge :: Int -> Bool
      notRightEdge x = (x + 1) `mod` len /= 0
      notTop x = (x - len) >= 0
      notBottom x = (x + len) < length grid
      result = mapMaybe (\f -> f index) [left, right, below, above, topLeft, topRight, bottomLeft, bottomRight]
  return result

parseGrid :: String -> OctopusGrid
parseGrid input = (length $ head $ lines input, griddata)
  where
    griddata = concatMap (map ((,False) . read . (: []))) (lines input)

raiseEnergy :: State OctopusGrid ()
raiseEnergy = do
  (len, grid) <- get
  put (len, map (\(x, y) -> (x + 1, y)) grid)

flash :: State OctopusGrid ()
flash = do
  (len, grid) <- get
  let nextFlash = findIndex (\(val, flashed) -> not flashed && val > 9) grid
      setFlashed :: Int -> OctopusGrid -> OctopusGrid
      setFlashed index (x, y) = (x, updateIndex index (\(val, _) -> (val, True)) y)
      energizeIndices :: [Int] -> OctopusGrid -> OctopusGrid
      energizeIndices indices (len, grid) = (len, foldl (\acc f -> f acc) grid (map (\x -> updateIndex x (\(x, y) -> (x + 1, y))) indices))
  case nextFlash of
    Just index -> do
      adj <- adjacent index
      put $ (energizeIndices adj . setFlashed index) (len, grid)
      flash
    Nothing -> return ()

countFlashed :: State OctopusGrid Int
countFlashed = do
  (len, grid) <- get
  let flashed = filter snd grid
  return $ length flashed

reset :: State OctopusGrid ()
reset = do
  (len, grid) <- get
  let newgrid = map (\(x, y) -> if y then (0, False) else (x, False)) grid
  put (len, newgrid)