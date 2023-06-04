module Part2 where

import Control.Monad.Trans.State
import Data.List (delete, find, insert)
import Data.Set qualified as Set

solvePart2 :: String -> IO ()
solvePart2 input = do
  let (nodes, (dimX, dimY)) = generateMap 1 $ parseNodes input
      initState = SolutionState {visited = Set.empty, unvisited = nodes, start = (0, 0), dest = (dimX - 1, dimY - 1), dimensions = (dimX, dimY)}
  putStrLn "Part 1"
  print $ evalState djikstra initState
  putStrLn "Part 2"
  let (nodes, (dimX, dimY)) = generateMap 5 $ parseNodes input
      initState2 = SolutionState {visited = Set.empty, unvisited = nodes, start = (0, 0), dest = (dimX - 1, dimY - 1), dimensions = (dimX, dimY)}
  print $ evalState djikstra initState2
  return ()

data Node = Node {coords :: (Int, Int), risk :: Int, distance :: Int} deriving (Show, Eq, Ord)

data SolutionState = SolutionState {unvisited :: Set.Set Node, start :: (Int, Int), dest :: (Int, Int), dimensions :: (Int, Int)} deriving (Show)

parseNodes :: String -> (Set.Set Node, (Int, Int))
parseNodes input = (Set.fromList $ map (\x -> Node {coords = x, risk = atCoord riskGrid x, distance = maxBound}) nodeCoords, (dimX, dimY))
  where
    parseDistance :: String -> [[Int]]
    parseDistance input = map (map (\x -> read [x])) $ lines input
    atCoord :: [[Int]] -> (Int, Int) -> Int
    atCoord grid (x, y) = ((!! x) . (!! y)) grid
    riskGrid = parseDistance input
    dimX = length (head riskGrid)
    dimY = length riskGrid
    nodeCoords = [(x, y) | x <- [0 .. dimX - 1], y <- [0 .. dimY - 1]]

generateMap :: Int -> (Set.Set Node, (Int, Int)) -> (Set.Set Node, (Int, Int))
generateMap blockDim (nodes, (dimX, dimY)) = (newNodes, (dimX * blockDim, dimY * blockDim))
  where
    newNodes = Set.fromList $ setStart $ map blockValue zippedNodes
    zippedNodes = [(n, c) | n <- Set.toList nodes, c <- [(x, y) | x <- [0 .. blockDim - 1], y <- [0 .. blockDim - 1]]]
    setStart :: [Node] -> [Node]
    setStart nodes = newNodes
      where
        Just startNode = find (\x -> coords x == (0, 0)) nodes
        newNodes = insert startNode {distance = 0} $ delete startNode nodes
    blockValue :: (Node, (Int, Int)) -> Node
    blockValue (node, (x, y)) = node {coords = newCoords, risk = newRisk}
      where
        (currentX, currentY) = coords node
        newCoords = (currentX + dimX * x, currentY + dimY * y)
        blockRisk = x + y
        newRisk = ((risk node - 1) + blockRisk) `mod` 9 + 1

neighbors :: Node -> State SolutionState (Set.Set Node)
neighbors node = do
  adjacentUnvisited $ coords node
  where
    adjacentUnvisited :: (Int, Int) -> State SolutionState (Set.Set Node)
    adjacentUnvisited (x, y) = do
      Set.filter (\x -> coords x `elem` possible) . unvisited <$> get
      where
        possible = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

contains :: Set.Set Node -> Node -> Bool
contains nodeSet node = any (\x -> coords x == coords node) nodeSet

update :: Node -> State SolutionState ()
update node = do
  s <- get
  if contains (visited s) node
    then put $ s {visited = updateNodeSet (visited s) node}
    else put $ s {unvisited = updateNodeSet (unvisited s) node}
  where
    updateNodeSet :: Set.Set Node -> Node -> Set.Set Node
    updateNodeSet nodeSet node = Set.insert node filtered
      where
        filtered = Set.filter (\x -> coords x /= coords node) nodeSet

nextCurrent :: State SolutionState Node
nextCurrent = do
  s <- get
  let unvisitedNodes = unvisited s
      nextCurrent = Set.fold (\node acc -> if distance node < distance acc then node else acc) Node {coords = (-1, -1), risk = 0, distance = maxBound} unvisitedNodes
  put $ s {visited = Set.insert nextCurrent (visited s), unvisited = Set.delete nextCurrent (unvisited s)}
  return nextCurrent

djikstra :: State SolutionState Node
djikstra = do
  current <- nextCurrent
  destination <- dest <$> get
  if coords current == destination
    then return current
    else do
      n <- neighbors current
      let updated = Set.map (updateDistance current) n
      mapM_ update $ Set.toList updated
      djikstra
  where
    updateDistance :: Node -> Node -> Node
    updateDistance src dest
      | distFromSrc < distance dest = dest {distance = distFromSrc}
      | otherwise = dest
      where
        distFromSrc = distance src + risk dest
