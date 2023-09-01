import System.IO
import Data.List
import Data.Maybe

type Position = Int
type Floor = Int

countParenthesis :: String -> Int
countParenthesis str = sum $ map upOrDown str

firstToReach :: String -> Floor -> Position
firstToReach str floor = 1 + (fromJust $ findIndex (== floor) $ scanl1 (+) $ map upOrDown str)

upOrDown :: Char -> Int
upOrDown '(' = 1
upOrDown ')' = -1
upOrDown _ = 0

main :: IO ()
main = do
  putStrLn "Puzzle answer for day 1, event 2015!\n"
  putStr "Insert filepath: "
  filePath <- getLine
  fileContents <- readFile filePath
  let lines' = lines fileContents
  let firstStar = countParenthesis (lines'!!0)
  putStrLn $ "First star: " ++ show firstStar
  let secondStar = firstToReach (lines'!!0) (-1)
  putStrLn $ "Second star: " ++ show secondStar
