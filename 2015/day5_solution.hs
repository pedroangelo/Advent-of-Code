import Data.List

containsVowels :: String -> Bool
containsVowels string = (<=) 3 $ length $ filter (True ==) $ map (\x -> elem x "aeiou") string

hasLetterTwiceRow :: String -> Bool
hasLetterTwiceRow string = any (True==) $ map (\x -> string!!(x-1) == string!!x) [1..length string-1]

notContainsStrings :: String -> Bool
notContainsStrings string = not $ any (True ==) $ filter (True ==) $ map (\x -> isInfixOf x string) ["ab","cd","pq","xy"]

isNiceString :: String -> Bool
isNiceString string = containsVowels string && hasLetterTwiceRow string && notContainsStrings string

getSubstrings :: String -> Bool
getSubstrings string = any (True==) $ map (\x -> isInfixOf (slice x (x+2) string) (slice (x+2) (length string) string)) [0..(length string)-2]

slice :: Int -> Int -> String -> String
slice start end = take (end-start) . drop start

hasLetterRepeatsTwiceBetween :: String -> Bool
hasLetterRepeatsTwiceBetween string = any (True==) $ map (\x -> string!!(x-2) == string!!x) [2..length string-1]

isNicerString :: String -> Bool
isNicerString string = getSubstrings string && hasLetterRepeatsTwiceBetween string

main :: IO ()
main = do
  putStrLn "Puzzle answer for day 5, event 2015!\n"
  putStr "Insert filepath: "
  filePath <- getLine
  fileContents <- readFile filePath
  let firstStar = length $ filter (True==) $ map isNiceString $ lines fileContents
  putStrLn $ "First star: " ++ show firstStar
  let secondStar = length $ filter (True==) $ map isNicerString $ lines fileContents
  putStrLn $ "Second star: " ++ show secondStar