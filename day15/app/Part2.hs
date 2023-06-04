module Part2 where

import Control.Monad.Trans.State
import Data.List (delete, find, insert, sortBy)
import Data.Maybe (fromJust, mapMaybe)
import Data.Set qualified as Set
import Data.Vector qualified as V

solvePart2 :: String -> IO ()
solvePart2 input = do
  let (nodes, (dimX, dimY)) = generateMap 1 $ parseNodes input
      initState = SolutionState {unvisited = V.fromList nodes, start = (0, 0), dest = (dimX - 1, dimY - 1), dimensions = (dimX, dimY)}
  putStrLn "Part 1"
  --   print $ runState (update Node {visited = True, coords = (0, 0), risk = 12, distance = 12}) initState
  --  print $ evalState (neighbors (head nodes)) initState
  print $ evalState djikstra initState
  putStrLn "Part 2"
  let (nodes, (dimX, dimY)) = generateMap 5 $ parseNodes input
      initState2 = SolutionState {unvisited = V.fromList nodes, start = (0, 0), dest = (dimX - 1, dimY - 1), dimensions = (dimX, dimY)}
  print $ evalState djikstra initState2
  return ()

data Node = Node {visited :: Bool, coords :: (Int, Int), risk :: Int, distance :: Int} deriving (Show, Eq, Ord)

data SolutionState = SolutionState {unvisited :: V.Vector Node, start :: (Int, Int), dest :: (Int, Int), dimensions :: (Int, Int)} deriving (Show)

parseNodes :: String -> ([Node], (Int, Int))
parseNodes input = (map (\x -> Node {coords = x, risk = atCoord riskGrid x, distance = maxBound, visited = False}) nodeCoords, (dimX, dimY))
  where
    parseDistance :: String -> [[Int]]
    parseDistance input = map (map (\x -> read [x])) $ lines input
    atCoord :: [[Int]] -> (Int, Int) -> Int
    atCoord grid (x, y) = ((!! x) . (!! y)) grid
    riskGrid = parseDistance input
    dimX = length (head riskGrid)
    dimY = length riskGrid
    nodeCoords = [(x, y) | x <- [0 .. dimX - 1], y <- [0 .. dimY - 1]]

generateMap :: Int -> ([Node], (Int, Int)) -> ([Node], (Int, Int))
generateMap blockDim (nodes, (dimX, dimY)) = (newNodes, (dimX * blockDim, dimY * blockDim))
  where
    maxDistance = sum $ replicate (dimX * blockDim * dimY * blockDim) 9
    setMaxDistance x = x {distance = maxDistance}
    newNodes = setStart $ sortBy (\x y -> compare (coords x) (coords y)) $ map (setMaxDistance . blockValue) zippedNodes
    zippedNodes = [(n, c) | n <- nodes, c <- [(x, y) | x <- [0 .. blockDim - 1], y <- [0 .. blockDim - 1]]]
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

neighbors :: Node -> State SolutionState [Node]
neighbors node = do
  adjacentUnvisited $ coords node
  where
    adjacentUnvisited :: (Int, Int) -> State SolutionState [Node]
    adjacentUnvisited (x, y) = do
      dim <- dimensions <$> get
      nodes <- unvisited <$> get
      let result = filter (not . visited) $ map (nodes V.!) (indices dim)
      return result
      where
        indices :: (Int, Int) -> [Int]
        indices dim = mapMaybe (coordToIndex dim) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

contains :: Set.Set Node -> Node -> Bool
contains nodeSet node = any (\x -> coords x == coords node) nodeSet

coordToIndex :: (Int, Int) -> (Int, Int) -> Maybe Int
coordToIndex (dimX, dimY) (x, y)
  | isValid = Just index
  | otherwise = Nothing
  where
    isValid = x < dimX && y < dimY && x >= 0 && y >= 0
    index = y + dimY * x

update :: Node -> State SolutionState ()
update node = do
  s <- get
  put $ s {unvisited = updateNodeSet (dimensions s) (unvisited s) node}
  where
    updateNodeSet :: (Int, Int) -> V.Vector Node -> Node -> V.Vector Node
    updateNodeSet dims nodeSet node = nodeSet V.// [(index, node)]
      where
        index = fromJust (coordToIndex dims $ coords node)

nextCurrent :: State SolutionState Node
nextCurrent = do
  s <- get
  let unvisitedNodes = V.filter (not . visited) $ unvisited s
      nextCurrent = foldl (\node acc -> if distance node < distance acc then node else acc) Node {visited = False, coords = (-1, -1), risk = 0, distance = maxBound} unvisitedNodes
  update nextCurrent {visited = True}
  --   put $ s {encountered = Set.delete (fromJust $ coordToIndex (dimensions s) $ coords nextCurrent) (encountered s)}
  return nextCurrent

djikstra :: State SolutionState Node
djikstra = do
  current <- nextCurrent
  destination <- dest <$> get
  if coords current == destination
    then return current
    else do
      n <- neighbors current
      s <- get
      -- let newIndices = Set.fromList $ map (\x -> fromJust $ coordToIndex (dimensions s) (coords x)) n
      -- put $ s {encountered = Set.union (encountered s) newIndices}
      let updated = map (updateDistance current) n
      mapM_ update updated
      djikstra
  where
    updateDistance :: Node -> Node -> Node
    updateDistance src dest
      | distFromSrc < distance dest = dest {distance = distFromSrc}
      | otherwise = dest
      where
        distFromSrc = distance src + risk dest
