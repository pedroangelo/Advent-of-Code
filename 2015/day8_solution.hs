import IOHandler

countString :: String -> Int
countString [] = 0
countString ('"' : string) = 0 + countString string
countString ('\\' : '\\' : string) = 1 + countString string
countString ('\\' : '\"' : string) = 1 + countString string
countString ('\\' : 'x' : c1 : c2 : string) = 1 + countString string
countString (c : string) = 1 + countString string

countString' :: String -> Int
countString' [] = 2
countString' ('"' : string) = 2 + countString' string
countString' ('\\' : '\\' : string) = 4 + countString' string
countString' ('\\' : '\"' : string) = 4 + countString' string
countString' ('\\' : 'x' : c1 : c2 : string) = 5 + countString' string
countString' (c : string) = 1 + countString' string

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput (PuzzleInfo "2015" "8")
  let strings = lines $ input
  let firstStar = sum $ map (\x -> (length x) - (countString x)) strings
  let secondStar = sum $ map (\x -> (countString' x) - (length x)) strings
  -- print puzzle results
  printPuzzleResults (PuzzleResult firstStar secondStar)
