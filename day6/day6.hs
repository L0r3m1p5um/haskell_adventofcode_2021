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

updateFishMap :: Map.Map Int Int -> Maybe (Map.Map Int Int)
updateFishMap fishmap = updateFishMap' (Just fishmap)
  where
    updateFishMap' fishmap' = do
      count8 <- Map.lookup 8 fishmap
      count7 <- Map.lookup 7 fishmap
      count6 <- Map.lookup 6 fishmap
      count5 <- Map.lookup 5 fishmap
      count4 <- Map.lookup 4 fishmap
      count3 <- Map.lookup 3 fishmap
      count2 <- Map.lookup 2 fishmap
      count1 <- Map.lookup 1 fishmap
      count0 <- Map.lookup 0 fishmap
      return
        ( Map.fromList
            [ (0, count1),
              (1, count2),
              (2, count3),
              (3, count4),
              (4, count5),
              (5, count6),
              (6, count7 + count0),
              (7, count8),
              (8, count0)
            ]
        )