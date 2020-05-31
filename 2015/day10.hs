import Data.List

-- build look-and-say sequences
lookAndSay :: String -> String
-- take the groups that make up the string (group equal chars), and for each group make a new sequence composed of the length and the number
lookAndSay string = concat $ map (\s -> (show $ length s) ++ [head s]) g
  where g = group string

main :: IO ()
main = do
  putStr "Puzzle input: "
  puzzleInput <- getLine
  let iterations = iterate lookAndSay puzzleInput
  putStrLn $ "First star: " ++ (show $ length $ head $ drop 40 $ iterations)
  putStrLn $ "Second star: " ++ (show $ length $ head $ drop 50 $ iterations)
  
