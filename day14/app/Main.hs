module Main where

import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Set qualified as Set
import System.Environment (getArgs)

main :: IO ()
main = do
  (filename : _) <- getArgs
  input <- readFile filename
  let template = head $ lines input
      rules1 = (Map.fromList . fst) $ parseLines (drop 2 $ lines input) parseRule1
      procWithRules = processTemplate1 rules1
  putStrLn "Part 1"
  print $ (diffCounts . countChars) $ foldr (\_ acc -> procWithRules acc) template [1 .. 10]
  putStrLn "Part 2"
  let templatePairs = parseTemplate $ head $ lines input
      lastChar = last template
      rules2 = (Map.fromList . fst) $ parseLines (drop 2 $ lines input) parseRule2
  print $ diffCounts $ countChars2 lastChar $ foldr (\_ acc -> processTemplate2 rules2 acc) templatePairs [1 .. 40]

parseLines :: [String] -> (String -> Maybe a) -> ([a], [String])
parseLines input parseLine = (parsed, drop (length parsed) input)
  where
    parsed = catMaybes $ takeWhile isJust (map parseLine input)

parseRule1 :: String -> Maybe (String, Char)
parseRule1 rule
  | length result == 2 = Just (head result, head $ result !! 1)
  | otherwise = Nothing
  where
    result = splitOn " -> " rule

processTemplate1 :: Map.Map String Char -> String -> String
processTemplate1 rules [x] = [x]
processTemplate1 rules (x : y : xs) = x : processRule x y : processTemplate1 rules (y : xs)
  where
    processRule x' y' = fromJust $ Map.lookup [x', y'] rules

countChars :: String -> Map.Map Char Int
countChars input = Map.fromSet countChar uniqueChars
  where
    uniqueChars = Set.fromList input
    countChar :: Char -> Int
    countChar char = length $ filter (== char) input

diffCounts :: Map.Map Char Int -> Int
diffCounts elements = maximum vals - minimum vals
  where
    vals = (map snd . Map.toList) elements

type PairCount = ((Char, Char), Int)

parseRule2 :: String -> Maybe ((Char, Char), Char)
parseRule2 rule
  | length result == 2 && length key == 2 = Just ((head key, key !! 1), head $ result !! 1)
  where
    result = splitOn " -> " rule
    key = head result

parseTemplate :: String -> [PairCount]
parseTemplate input = consolidate $ map (,1) $ parseTemplate' input
  where
    parseTemplate' :: String -> [(Char, Char)]
    parseTemplate' [_] = []
    parseTemplate' (x : y : xs) = (x, y) : parseTemplate' (y : xs)

consolidate :: [PairCount] -> [PairCount]
consolidate xs = map (\key -> (key, keySum key xs)) keys
  where
    keys = Set.toList $ Set.fromList $ map fst xs
    keySum :: (Char, Char) -> [PairCount] -> Int
    keySum key list = sum $ map snd $ filter (\x -> fst x == key) list

applyRule :: Map.Map (Char, Char) Char -> PairCount -> [PairCount]
applyRule rules (pair, count) = [((fst pair, ruleResult), count), ((ruleResult, snd pair), count)]
  where
    ruleResult = fromJust $ Map.lookup pair rules

processTemplate2 :: Map.Map (Char, Char) Char -> [PairCount] -> [PairCount]
processTemplate2 rules template = consolidate $ concatMap (applyRule rules) template

countChars2 :: Char -> [PairCount] -> Map.Map Char Int
countChars2 endChar input = Map.fromSet countChar uniqueChars
  where
    uniqueChars = Set.fromList $ map (fst . fst) input
    countChar x = if x == endChar then 1 + countChar' x else countChar' x
    countChar' :: Char -> Int
    countChar' char = sum $ map snd $ filter (\x -> (fst . fst) x == char) input