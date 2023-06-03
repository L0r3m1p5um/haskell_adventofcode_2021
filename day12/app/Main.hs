module Main where

import Data.Char (toLower)
import Data.List.Split
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import System.Environment

main :: IO ()
main = do
  (filename : _) <- getArgs
  input <- readFile filename
  let edges = parseEdges input
  print "Part 2"
  print $ length $ buildPaths edges

data Size = Small | Large deriving (Show, Eq, Ord)

type Node = (String, Size)

type Edge = (Node, Node)

parseNode :: String -> Node
parseNode node
  | node == map toLower node = (node, Small)
  | otherwise = (node, Large)

parseEdge :: String -> Edge
parseEdge edge = (parseNode src, parseNode dest)
  where
    nodes = splitOn "-" edge
    src = head nodes
    dest = nodes !! 1

parseEdges :: String -> [Edge]
parseEdges input = map parseEdge $ lines input

type Path = [Node]

isFinal :: Path -> Bool
isFinal path = (fst . head) path == "end"

traverseEdge :: Path -> Edge -> Maybe Path
traverseEdge path (x, y)
  | isValidEdge (x, y) = Just $ y : path
  | isValidEdge (y, x) = Just $ x : path
  | otherwise = Nothing
  where
    lastNode = head path
    isValidEdge :: Edge -> Bool
    isValidEdge (src, (_, Large)) = src == lastNode
    isValidEdge (src, (dest, Small)) = dest /= "start" && (not hasVisitedSmallTwice || (dest, Small) `notElem` path) && src == lastNode
    -- Part 1: isValidEdge (src, (dest, Small)) = (dest, Small) `notElem` path && src == lastNode
    smallNodes = filter (\(_, size) -> size == Small) path
    hasVisitedSmallTwice :: Bool
    hasVisitedSmallTwice = length smallNodes /= length (Set.fromList smallNodes)

buildPaths :: [Edge] -> [Path]
buildPaths edges = buildPaths' [[("start", Small)]]
  where
    buildPathStep :: [Path] -> [Path]
    buildPathStep paths = finalPaths ++ catMaybes [f x | f <- map traverseEdge notFinalPaths, x <- edges]
      where
        finalPaths = filter isFinal paths
        notFinalPaths = filter (not . isFinal) paths
    buildPaths' :: [Path] -> [Path]
    buildPaths' paths
      | all isFinal paths = paths
      | otherwise = buildPaths' (buildPathStep paths)