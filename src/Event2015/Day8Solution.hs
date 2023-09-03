module Event2015.Day8Solution (main, solve) where

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

-- MAIN FUNCTIONS

solve :: String -> (String, String)
solve input = (firstStar, secondStar)
  where strings = lines $ input
        firstStar = show $ sum $ map (\x -> (length x) - (countString x)) strings
        secondStar = show $ sum $ map (\x -> (countString' x) - (length x)) strings

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput "2015" "8"
  let (firstStar, secondStar) = solve input
  -- print puzzle results
  printPuzzleResults firstStar secondStar
