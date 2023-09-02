module IOHandler where

import Data.Char

type Year = String
type Day = String
type PuzzleInput = String

standardFilePath :: Day -> String
standardFilePath day = "day" ++ day ++ "_input"

-- helper function to print presentation message and handle input retrieval
obtainPuzzleInput :: Year -> Day -> IO PuzzleInput
obtainPuzzleInput year day = do
  putStrLn $ "Puzzle answer for day " ++ day ++ ", event " ++ year ++ "!\n"
  putStr "Insert filepath (press RET for standard input): "
  userInput <- getLine
  let filePath = if userInput == "" then (standardFilePath day) else userInput
  fileContents <- readFile filePath
  return fileContents

-- helper function to print puzzle results
printPuzzleResults :: (Show a, Show b) => a -> b -> IO ()
printPuzzleResults firstStar secondStar = do
  putStrLn $ "First star: " ++ show firstStar
  putStrLn $ "Second star: " ++ show secondStar
