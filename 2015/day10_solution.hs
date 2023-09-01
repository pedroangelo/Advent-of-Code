import Data.List

-- build look-and-say sequences
lookAndSay :: String -> String
-- take the groups that make up the string (group equal chars), and for each group make a new sequence composed of the length and the number
lookAndSay string = concat $ map (\s -> (show $ length s) ++ [head s]) g
  where g = group string

main :: IO ()
main = do
  putStrLn "Puzzle answer for day 10, event 2015!\n"
  putStr "Insert filepath: "
  filePath <- getLine
  fileContents <- readFile filePath
  let lines' = Data.List.lines fileContents
  let iterations = iterate lookAndSay $ lines'!!0
  let firstStar = length $ iterations!!40
  putStrLn $ "First star: " ++ show firstStar
  let secondStar = length $ iterations!!50
  putStrLn $ "Second star: " ++ show secondStar
  
