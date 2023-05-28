module Main where

import Control.Monad (foldM)
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import System.Environment (getArgs)

main :: IO ()
main = do
  (filename : _) <- getArgs
  input <- readFile filename
  let displays = parseInput input
  print "Part 1"
  print $ solvePart1 displays
  print "Part 2"
  let displayKeys = map (\x -> ((decodeDisplay . pattern) x, output x)) displays
  print $ sum $ map read $ mapMaybe (\x -> decodeOutput (fst x) (snd x)) displayKeys

newtype Display = Display ([String], [String]) deriving (Show)

output :: Display -> [String]
output (Display (_, x)) = x

pattern :: Display -> [String]
pattern (Display (x, _)) = x

parseInput :: String -> [Display]
parseInput input = map parseDisplay (lines input)
  where
    parseDisplay :: String -> Display
    parseDisplay display =
      let numList = splitOn " " . trim <$> splitOn "|" display
       in Display (head numList, numList !! 1)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

isUniqueDigit :: String -> Bool
isUniqueDigit x = len == 2 || len == 4 || len == 3 || len == 7
  where
    len = length x

solvePart1 :: [Display] -> Int
solvePart1 displays = length $ concatMap (filter isUniqueDigit . output) displays

type DisplayKey = Map.Map Char (Set.Set Char)

decodeDisplay :: [String] -> DisplayKey
decodeDisplay patterns = matchRest (matchUnique patterns) patterns
  where
    uniqueToKey :: String -> Maybe (Char, Set.Set Char)
    uniqueToKey x
      | length x == 2 = Just ('1', Set.fromList x)
      | length x == 3 = Just ('7', Set.fromList x)
      | length x == 4 = Just ('4', Set.fromList x)
      | length x == 7 = Just ('8', Set.fromList x)
      | otherwise = Nothing
    matchWithUnique :: DisplayKey -> String -> Maybe (Char, Set.Set Char)
    matchWithUnique displayKey pattern
      | isUniqueDigit pattern = uniqueToKey pattern
      | p4 `Set.isSubsetOf` pSet = Just ('9', pSet)
      | length pattern == 6 && p7 `Set.isSubsetOf` pSet = Just ('0', pSet)
      | length pattern == 6 = Just ('6', pSet)
      | length pattern == 5 && p7 `Set.isSubsetOf` pSet = Just ('3', pSet)
      | Set.size (Set.union pSet p4) == 7 = Just ('2', pSet)
      | length pattern == 5 = Just ('5', pSet)
      | otherwise = Nothing
      where
        pSet = Set.fromList pattern
        Just p1 = Map.lookup '1' displayKey
        Just p4 = Map.lookup '4' displayKey
        Just p7 = Map.lookup '7' displayKey
        Just p8 = Map.lookup '8' displayKey
    matchUnique :: [String] -> DisplayKey
    matchUnique xs = Map.fromList $ mapMaybe uniqueToKey xs
    matchRest :: DisplayKey -> [String] -> DisplayKey
    matchRest displayKey patterns = Map.fromList $ mapMaybe (matchWithUnique displayKey) patterns

decodeOutput :: DisplayKey -> [String] -> Maybe String
decodeOutput displayKey patterns = foldl (addCharacter flippedDisplay) (return "") pSet
  where
    pSet = map Set.fromList patterns
    flippedDisplay = Map.fromList $ map (\(x, y) -> (y, x)) (Map.toList displayKey)
    addCharacter :: Map.Map (Set.Set Char) Char -> Maybe String -> Set.Set Char -> Maybe String
    addCharacter displayMap start pattern = do
      newChar <- Map.lookup pattern displayMap
      acc <- start
      return (acc ++ [newChar])