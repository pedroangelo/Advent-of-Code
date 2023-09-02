import Data.List
--import Data.List.Split
import IOHandler

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

getChunks :: [a] -> [[a]]
getChunks [] = []
getChunks (x : []) = [[x]]
getChunks (x:y:ls) = [x,y] : getChunks ls

splitMoves :: [Move] -> ([Move], [Move])
splitMoves moves = unzip $ map (\move -> (head move, head $ tail move)) $ getChunks moves


main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput "2015" "3"
  let moves = map textToMoves input
  let santaPath = buildPath moves
  let firstStar = length $ nub santaPath
  
  let (santaMoves, roboSantaMoves) = splitMoves moves
  let santaPath2 = buildPath santaMoves
  let roboSantaPath2 = buildPath roboSantaMoves
  let secondStar = length $ nub $ santaPath2 ++ roboSantaPath2
  -- print puzzle results
  printPuzzleResults firstStar secondStar
