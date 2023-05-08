import Data.List (group, sortBy)
import Data.List.Split (splitOn)
import System.Environment

main = do
  (filename : _) <- getArgs
  input <- readFile filename
  let values = filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2) $ map parseLine' (lines input)
      segments = concatMap expandLine values
      counts = (filter (\x -> length x > 1) . group . sortBy orderSegments) segments
  putStrLn "Part 1"
  print (length counts)
  let values = map parseLine' (lines input)
      segments = concatMap expandLine values
      counts = (filter (\x -> length x > 1) . group . sortBy orderSegments) segments
  putStrLn "Part 2"
  print (length counts)

orderSegments :: (Int, Int) -> (Int, Int) -> Ordering
orderSegments (x1, y1) (x2, y2)
  | x1 > x2 = GT
  | x1 < x2 = LT
  | x1 == x2 && y1 > y2 = GT
  | x1 == x2 && y2 > y1 = LT
  | x1 == x2 && y1 == y2 = EQ

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine line = (parsePoint start, parsePoint end)
  where
    (start : x : end : xs) = words line
    parsePoint :: String -> (Int, Int)
    parsePoint point = head $ map ((\(x : y : xs) -> (x, y)) . read) (splitOn "," point)

parseLine' :: String -> ((Int, Int), (Int, Int))
parseLine' line = (parsePoint start, parsePoint end)
  where
    (start : x : end : xs) = words line
    parsePoint :: String -> (Int, Int)
    parsePoint point = (\(x : y : _) -> (x, y)) $ map read (splitOn "," point)

expandLine :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
expandLine ((x1, y1), (x2, y2))
  | x1 == x2 && y1 <= y2 = map (x1,) [y1 .. y2]
  | x1 == x2 && y1 >= y2 = map (x1,) [y2 .. y1]
  | y1 == y2 && x1 <= x2 = map (,y1) [x1 .. x2]
  | y1 == y2 && x1 >= x2 = map (,y1) [x2 .. x1]
  | x1 <= x2 && y1 <= y2 = zip [x1 .. x2] [y1 .. y2]
  | x2 < x1 && y1 <= y2 = zip (reverse [x2 .. x1]) [y1 .. y2]
  | x1 <= x2 && y2 < y1 = zip [x1 .. x2] (reverse [y2 .. y1])
  | x2 <= x1 && y2 < y1 = zip [x2 .. x1] [y2 .. y1]