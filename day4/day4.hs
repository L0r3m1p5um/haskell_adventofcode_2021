import Data.List (elemIndex, find, findIndex, transpose)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

main = do
  (filename : _) <- getArgs
  input <- readFile filename
  let (boardInput, numbers) = parseDrawnNumbers input
      boards = parseBoards boardInput
      boardStates = map (\board -> scanl (\acc mark -> mark acc) board (map markBoard numbers)) boards
      winningIndices = map ((\(Just x) -> x) . findIndex checkWinner) boardStates
      winningTurn = minimum winningIndices
      Just winningBoardIndex = elemIndex winningTurn winningIndices
      winningBoard = zip (-1 : numbers) (boardStates !! winningBoardIndex) !! winningTurn
      lastWinningTurn = maximum winningIndices
      Just lastWinningBoardIndex = elemIndex lastWinningTurn winningIndices
      lastWinningBoard = zip (-1 : numbers) (boardStates !! lastWinningBoardIndex) !! lastWinningTurn
  putStrLn "Part 1"
  print (fst winningBoard * (scoreBoard . snd) winningBoard)
  putStrLn "Part 2"
  print (fst lastWinningBoard * (scoreBoard . snd) lastWinningBoard)

parseDrawnNumbers :: String -> (String, [Int])
parseDrawnNumbers input = (unlines xs, numbers)
  where
    (line : _ : xs) = lines input
    numbers = map read (splitOn "," line)

type BingoBoard = [[(Int, Bool)]]

parseBingoBoard :: String -> (String, BingoBoard)
parseBingoBoard input = (rest, map parseLine boardLines)
  where
    boardLines = take 5 $ lines input
    rest = unlines $ drop 6 $ lines input
    parseLine :: String -> [(Int, Bool)]
    parseLine = map ((,False) . read) . words

parseBoards :: String -> [BingoBoard]
parseBoards "" = []
parseBoards input = board : parseBoards rest
  where
    (rest, board) = parseBingoBoard input

checkWinner :: BingoBoard -> Bool
checkWinner board = any (all snd) rowsAndCols
  where
    rowsAndCols = board ++ transpose board

markBoard :: Int -> BingoBoard -> BingoBoard
markBoard number = map (markRow number)
  where
    markRow :: Int -> [(Int, Bool)] -> [(Int, Bool)]
    markRow x = map (\(num, marked) -> if num == x then (num, True) else (num, marked))

scoreBoard :: BingoBoard -> Int
scoreBoard board = sum $ map fst $ filter (not . snd) $ concat board