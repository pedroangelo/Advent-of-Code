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

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput "2015" "1"
  let lines' = lines input
  let firstStar = countParenthesis (lines'!!0)
  let secondStar = firstToReach (lines'!!0) (-1)
  -- print puzzle results
  printPuzzleResults firstStar secondStar
