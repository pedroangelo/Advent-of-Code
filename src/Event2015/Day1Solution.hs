module Event2015.Day1Solution (main, solve) where

import Data.List
import Data.Maybe
import IOHandler

type Position = Int
type Floor = Int

upOrDown :: Char -> Int
upOrDown '(' = 1
upOrDown ')' = -1
upOrDown _ = 0

countParenthesis :: String -> Int
countParenthesis str = sum $ map upOrDown str

firstToReach :: String -> Floor -> Position
firstToReach str floor = 1 + (fromJust $ findIndex (== floor) $ scanl1 (+) $ map upOrDown str)

-- MAIN FUNCTIONS

solve :: String -> (String, String)
solve input = (firstStar, secondStar)
  where lines' = lines input
        firstStar = show $ countParenthesis (lines'!!0)
        secondStar = show $ firstToReach (lines'!!0) (-1)

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput "2015" "1"
  let (firstStar, secondStar) = solve input
  -- print puzzle results
  printPuzzleResults firstStar secondStar
