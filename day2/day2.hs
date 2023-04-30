import System.Environment (getArgs)

main = do
  (filename : _) <- getArgs
  input <- readFile filename
  let commands = map (parseCommand . words) $ lines input
      (h1_final, d1_final) = foldr (\(command, distance) acc -> updatePosition command distance acc) (0, 0) commands
      (h2_final, d2_final, aim_final) = foldl (\acc (command, distance) -> updatePositionAim command distance acc) (0, 0, 0) commands
  putStrLn "Part 1"
  print $ h1_final * d1_final
  putStrLn "Part 2"
  print $ h2_final * d2_final

data Command = Forward | Down | Up deriving (Show)

type Position = (Int, Int)

type PositionAim = (Int, Int, Int)

updatePosition :: Command -> Int -> Position -> Position
updatePosition Forward distance (x, y) = (x + distance, y)
updatePosition Down distance (x, y) = (x, y + distance)
updatePosition Up distance (x, y) = (x, y - distance)

parseCommand :: [String] -> (Command, Int)
parseCommand (command : distance : _) = case command of
  "forward" -> (Forward, read distance)
  "up" -> (Up, read distance)
  "down" -> (Down, read distance)

updatePositionAim :: Command -> Int -> PositionAim -> PositionAim
updatePositionAim Forward distance (horizontal, depth, aim) = (horizontal + distance, depth + (aim * distance), aim)
updatePositionAim Up distance (horizontal, depth, aim) = (horizontal, depth, aim - distance)
updatePositionAim Down distance (horizontal, depth, aim) = (horizontal, depth, aim + distance)