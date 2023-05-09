import Data.List.Split (splitOn)
import Data.Map qualified as Map
import System.Environment
import System.IO

main = do
  (filename : _) <- getArgs
  input <- readFile filename
  let fish :: [Int] = read <$> splitOn "," input
  putStrLn "Part 1"
  print $ length (simulate 80 fish)
  putStrLn "Part 2"
  let fishmap = fishMap fish
  let (Just simFish) = simulate' 256 (return fishmap)
  print $ Map.foldr (+) 0 simFish

spawnFish :: [Int] -> [Int]
spawnFish = countdown . spawnFish'
  where
    countdown = map (\x -> if x == 0 then 6 else x - 1)
    spawnFish' xs = xs ++ replicate (length (filter (== 0) xs)) 9

simulate :: Int -> [Int] -> [Int]
simulate 0 fish = fish
simulate n fish = simulate (n - 1) (spawnFish fish)

fishMap :: [Int] -> Map.Map Int Int
fishMap [] =
  Map.fromList
    [ (0, 0),
      (1, 0),
      (2, 0),
      (3, 0),
      (4, 0),
      (5, 0),
      (6, 0),
      (7, 0),
      (8, 0),
      (9, 0)
    ]
fishMap (x : xs) = Map.insert x (lookupOrDefault x (fishMap xs) + 1) (fishMap xs)
  where
    lookupOrDefault x map = case (Map.lookup x map) of
      Nothing -> 0
      Just result -> result

simulate' :: Int -> Maybe (Map.Map Int Int) -> Maybe (Map.Map Int Int)
simulate' _ Nothing = Nothing
simulate' 0 fishmap = fishmap
simulate' n fishmap = simulate' (n - 1) fishmap >>= updateFishMap

extractMaybeList :: [Maybe a] -> Maybe [a]
extractMaybeList [] = Just []
extractMaybeList (Nothing : xs) = Nothing
extractMaybeList ((Just x) : xs) = Just (x :) <*> extractMaybeList xs

updateFishMap :: Map.Map Int Int -> Maybe (Map.Map Int Int)
updateFishMap fishmap = updateFishMap' (Just fishmap)
  where
    updateFishMap' fishmap' = do
      counts <- extractMaybeList $ map (\x -> Map.lookup x fishmap) [0 .. 8]
      let newFish = map (\x -> (x, (if x == 6 then head counts else 0) + counts !! ((x + 1) `mod` 9))) [0 .. 8]
      return (Map.fromList newFish)