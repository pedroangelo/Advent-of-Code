import Data.Char (isDigit)

retrieveNumbers :: String -> [String]
retrieveNumbers [] = []
retrieveNumbers string = number : retrieveNumbers restOfString
  where (_, leadingWithNumber) = span (\x -> not $ (isDigit x) || (x == '-')) string
        (number, restOfString) = span (\x -> (isDigit x) || (x == '-')) leadingWithNumber

main :: IO ()
main = do
  putStr "Filepath: "
  filePath <- getLine
  fileContents <- readFile filePath
  putStrLn $ "First Star: " ++ (show $ sum $ map (\x -> read x :: Int) $ init $ retrieveNumbers fileContents)
