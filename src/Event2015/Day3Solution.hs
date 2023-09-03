module Event2015.Day3Solution (main, solve) where

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

-- MAIN FUNCTIONS

solve :: String -> (String, String)
solve input = (firstStar, secondStar)
  where moves = map textToMoves input
        santaPath = buildPath moves
        firstStar = show $ length $ nub santaPath
  
        (santaMoves, roboSantaMoves) = splitMoves moves
        santaPath2 = buildPath santaMoves
        roboSantaPath2 = buildPath roboSantaMoves
        secondStar = show $ length $ nub $ santaPath2 ++ roboSantaPath2

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput "2015" "3"
  let (firstStar, secondStar) = solve input
  -- print puzzle results
  printPuzzleResults firstStar secondStar
