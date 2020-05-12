import Data.List
import Data.List.Split

type Move = (Int, Int)
type Step = (Int, Int)
type Path = [Step]

textToMoves :: Char -> Move
textToMoves char
  | char == '^' = (0,  1)
  | char == 'v' = (0, -1)
  | char == '>' = ( 1, 0)
  | char == '<' = (-1, 0)

buildPath :: [Move] -> Path
buildPath moves = scanl (\(x1,y1) -> \(x2,y2) -> (x1 + x2, y1 + y2)) (0,0) moves

splitMoves :: [Move] -> ([Move], [Move])
splitMoves moves = unzip $ map (\move -> (head move, head $ tail move)) $ chunksOf 2 moves

main :: IO ()
main = do
  putStr "Filepath: "
  filePath <- getLine
  fileContents <- readFile filePath
  let text = (lines fileContents)!!0
  let moves = map textToMoves text
  let (moves1, moves2) = splitMoves moves
  putStrLn $ "Houses visited: " ++ (show $ housesVisited $ buildPath $ moves)
  putStrLn $ "Houses visited by Santa: " ++ (show $ length $ nub $ buildPath moves1)
  putStrLn $ "Houses visited by Robo-Santa: " ++ (show $ length $ nub $ buildPath moves2)
  putStrLn $ "Houses visited by either Santa and Robo-Santa: " ++ (show $ length $ nub $ (buildPath moves1 ++ buildPath moves2))
