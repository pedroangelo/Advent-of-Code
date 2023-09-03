module Event2015.Day10Solution (main, solve) where

import Data.List
import IOHandler

-- build look-and-say sequences
lookAndSay :: String -> String
-- take the groups that make up the string (group equal chars), and for each group make a new sequence composed of the length and the number
lookAndSay string = concat $ map (\s -> (show $ length s) ++ [head s]) g
  where g = group string

-- MAIN FUNCTIONS

solve :: String -> (String, String)
solve input = (firstStar, secondStar)
  where lines' = Data.List.lines input
        iterations = iterate lookAndSay $ lines'!!0
        firstStar = show $ length $ iterations!!40
        secondStar = show $ length $ iterations!!50

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput "2015" "10"
  let (firstStar, secondStar) = solve input
  -- print puzzle results
  printPuzzleResults firstStar secondStar
