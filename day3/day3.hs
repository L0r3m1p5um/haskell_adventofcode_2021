import System.Environment (getArgs)

main = do
  (filename : _) <- getArgs
  input <- readFile filename
  let values = lines input
      numLen = length $ head values
      bitPositions = map (\x -> map (!! x) values) [0 .. (numLen - 1)]
      gamma = concatMap ((\(x, y) -> if x > y then "0" else "1") . digitCount) bitPositions
      epsilon = concatMap ((\(x, y) -> if x > y then "1" else "0") . digitCount) bitPositions
      gammaDec = binToDec gamma
      epsilonDec = binToDec epsilon
      oxygen = binToDec $ bitFilter bitCriteriaOxygen values
      co2 = binToDec $ bitFilter bitCriteriaCo2 values
  print (gammaDec * epsilonDec)
  putStrLn "Part 2"
  print $ co2 * oxygen

digitCount :: String -> (Int, Int)
digitCount = digitCount' (0, 0)
  where
    digitCount' :: (Int, Int) -> String -> (Int, Int)
    digitCount' (x, y) [] = (x, y)
    digitCount' (x, y) ('1' : xs) = digitCount' (x, y + 1) xs
    digitCount' (x, y) ('0' : xs) = digitCount' (x + 1, y) xs

binToDec :: String -> Int
binToDec = binToDec' . reverse
  where
    binToDec' :: String -> Int
    binToDec' [] = 0
    binToDec' (x : xs) = read [x] + 2 * binToDec' xs

bitFilter :: (Int -> [String] -> Char) -> [String] -> String
bitFilter criteria values = bitFilter' criteria 0 values
  where
    bitFilter' :: (Int -> [String] -> Char) -> Int -> [String] -> String
    bitFilter' _ _ [x] = x
    bitFilter' criteria index values = bitFilter' criteria (index + 1) (filterByCriteria criteria index values)
    filterByCriteria :: (Int -> [String] -> Char) -> Int -> [String] -> [String]
    filterByCriteria criteria index values = filter (\x -> criteria index values == x !! index) values

digitCountAtIndex :: Int -> [String] -> (Int, Int)
digitCountAtIndex index values = digitCount indexString
  where
    indexString = map (!! index) values

bitCriteriaOxygen :: Int -> [String] -> Char
bitCriteriaOxygen = bitCriteria (\(x, y) -> if x > y then '0' else '1')

bitCriteriaCo2 :: Int -> [String] -> Char
bitCriteriaCo2 = bitCriteria (\(x, y) -> if x > y then '1' else '0')

bitCriteria :: ((Int, Int) -> Char) -> Int -> [String] -> Char
bitCriteria f index values = f digitCounts
  where
    digitCounts = digitCountAtIndex index values