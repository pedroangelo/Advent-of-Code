module IOHandler where

import Data.Char

type Year = String
type Day = String

data PuzzleInfo = PuzzleInfo Year Day
type PuzzleInput = String

data PuzzleResult a b = PuzzleResult a b

standardFilePath :: Day -> String
standardFilePath day = "day" ++ day ++ "_input"

-- helper function to print presentation message and handle input retrieval
obtainPuzzleInput :: PuzzleInfo -> IO PuzzleInput
obtainPuzzleInput (PuzzleInfo year day) = do
  putStrLn $ "Puzzle answer for day " ++ day ++ ", event " ++ year ++ "!\n"
  putStr "Insert filepath (press RET for standard input): "
  userInput <- getLine
  let filePath = if userInput == "" then (standardFilePath day) else userInput
  fileContents <- readFile filePath
  return fileContents

-- helper function to print puzzle results
printPuzzleResults :: (Show a, Show b) => PuzzleResult a b -> IO ()
printPuzzleResults (PuzzleResult firstStar secondStar) = do
  putStrLn $ "First star: " ++ show firstStar
  putStrLn $ "Second star: " ++ show secondStar
