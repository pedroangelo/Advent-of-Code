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
  putStr "Filepath: "
  filePath <- getLine
  fileContents <- readFile filePath
  let strings = lines $ fileContents
  let count = sum $ map (\x -> (length x) - (countString x)) strings
  let count2 = sum $ map (\x -> (countString' x) - (length x)) strings
  putStrLn $ "First star: " ++ (show count)
  putStrLn $ "Second star: " ++ (show count2)
