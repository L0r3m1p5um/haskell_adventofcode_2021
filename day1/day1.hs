import System.Environment (getArgs)

main = do
  (fileName : _) <- getArgs
  input <- readFile fileName
  let (depths :: [Int]) = map read $ lines input
  putStrLn "Part 1"
  print $ length $ filter (\[x, y] -> y > x) $ window 2 depths
  putStrLn "Part 2"
  print $ length $ filter (\[x, y] -> y > x) $ window 2 $ map sum $ window 3 depths

window :: Int -> [a] -> [[a]]
window size (x : xs)
  | size <= length xs + 1 = take size (x : xs) : window size xs
  | otherwise = []
