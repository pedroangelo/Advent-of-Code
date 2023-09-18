module Event2015.Day12Solution (main, solve) where

import Data.Char (isDigit, isNumber)
import Data.List (span)
import IOHandler

data Json
  = String String
  | Number Int
  | Array [Json]
  | Object [(String, Json)]
  deriving (Show, Eq)

-- read all numbers from a string
-- function only used by easy solution for first star
retrieveNumbers :: String -> [String]
retrieveNumbers [] = []
retrieveNumbers string = number : retrieveNumbers restOfString
  where
        -- ignore text until a number is found, return text starting with the number
        (_, leadingWithNumber) = span (\x -> not $ (isDigit x) || (x == '-')) string
        -- extract both the number from the string, as well as the rest of the text
        (number, restOfString) = span (\x -> (isDigit x) || (x == '-')) leadingWithNumber

-- parse a string
parseString :: String -> (String, String)
parseString text =
  let (string, restOfText) = span ('\"' /=) $ tail text
  in (string, tail restOfText)

-- parse a number
parseNumber :: String -> (Int, String)
parseNumber text =
  let
    (mul, text') = if head text == '-' then (-1, tail text) else (1, text)
    (number, restOfText) = span isNumber text'
  in (mul * (read number), restOfText)
  
-- parse an array
parseArray :: String -> ([Json], String)
parseArray text
  -- list is over
  | head text == ']' = ([], tail text)
  -- parse next element, and recursively parse next list segment
  | head text == '[' || head text == ',' =
    let
      (subJson, restOfText) = parseJson $ tail text
      (subJson', restOfText') = parseArray $ restOfText
    in (subJson : subJson', restOfText')

-- parse an object
parseObject text
  -- object is over
  | head text == '}' = ([], tail text)
  -- parse next element, and recursively parse next object segment
  | head text == '{' || head text == ',' =
    let
      (key, restOfText) = parseString $ tail text
      (value, restOfText') = parseJson $ tail restOfText
      (subJson, restOfText'') = parseObject $ restOfText'
    in ((key, value) : subJson, restOfText'')

-- parse a Json document
parseJson :: String -> (Json, String)
parseJson text
  -- parsing a string
  | head text == '\"' =
    let (string, restOfText) = parseString text
    in (String string, restOfText)
  -- parsing a number
  | isNumber (head text) || head text == '-' =
    let (number, restOfText) = parseNumber text
    in (Number number, restOfText)
  -- parsing an array
  | head text == '[' =
    let (array, restOfText) = parseArray text
    in (Array array, restOfText)
  -- parsing an object
  | head text == '{' =
    let (object, restOfText) = parseObject text
    in (Object object, restOfText)

-- sum all the numbers in Json
countJson :: Json -> Int
countJson (String _) = 0
countJson (Number number) = number
countJson (Array array) = sum $ map countJson array
countJson (Object object) = sum $ map (\x -> countJson $ snd x) object

-- remove objects with red
removeRed :: Json -> [Json]
removeRed (String string) = [String string]
removeRed (Number number) = [Number number]
removeRed (Array array) =
  let
    -- filter top level
    array' = filter (not . hasRed) array
    -- filter recursively
    array'' = concat $ map removeRed array'
  in [Array array'']
removeRed (Object object)
  | hasRed $ Object object = []
  | otherwise = 
    let object' = map (\x -> (fst x, head $ snd x)) $ filter (\x -> (length $ snd x) /= 0) $ map (\x -> (fst x, removeRed $ snd x)) object
    in [Object object']
  
-- check if an object has a red value
hasRed :: Json -> Bool
hasRed (String string) = string == "red"
hasRed (Number _) = False
hasRed (Array array) = False
hasRed (Object object) = or $ map ((== String "red") . snd) object


-- MAIN FUNCTIONS

solve :: String -> (String, String)
solve input = (firstStar, secondStar)
  where -- simple solution for first star,
        -- retrieve all numbers in input, read them as int and add them all together
        firstStarSimple = show $ sum $ map (\x -> read x :: Int) $ init $ retrieveNumbers input

        -- general solution, for both first and second star
        json = fst $ parseJson input
        -- sum all numbers
        firstStar = show $ countJson json
        -- sum all numbers not on objects with the "red" property
        secondStar = show $ countJson $ head $ removeRed json

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput "2015" "12"
  let (firstStar, secondStar) = solve input
  -- print puzzle results
  printPuzzleResults firstStar secondStar
