import Data.List
import IOHandler

-- build look-and-say sequences
lookAndSay :: String -> String
-- take the groups that make up the string (group equal chars), and for each group make a new sequence composed of the length and the number
lookAndSay string = concat $ map (\s -> (show $ length s) ++ [head s]) g
  where g = group string

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput (PuzzleInfo "2015" "10")
  let lines' = Data.List.lines input
  let iterations = iterate lookAndSay $ lines'!!0
  let firstStar = length $ iterations!!40
  let secondStar = length $ iterations!!50
  -- print puzzle results
  printPuzzleResults (PuzzleResult firstStar secondStar)
