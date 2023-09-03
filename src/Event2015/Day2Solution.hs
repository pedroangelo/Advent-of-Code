module Event2015.Day2Solution (main, solve) where

import Data.List
import Data.Text
import IOHandler

type Length = Int
type Width = Int
type Height = Int
type Present = (Length, Width, Height)
type OrderedPresent = [Int]

presentToList :: Present -> OrderedPresent
presentToList (l, w, h) = sortBy (\x -> \y -> compare x y) [l,w,h]

presentSide :: OrderedPresent -> Int -> Int
presentSide orderedPresent index = orderedPresent!!index

surfaceArea :: OrderedPresent -> Int
surfaceArea present = 2*s0*s1 + 2*s1*s2 + 2*s2*s0
  where s0 = presentSide present 0
        s1 = presentSide present 1
        s2 = presentSide present 2

smallestSideArea :: OrderedPresent -> Int
smallestSideArea present = (presentSide present 0) * (presentSide present 1)

paperNeeded :: OrderedPresent -> Int
paperNeeded present = surfaceArea present + smallestSideArea present

textToPresent :: String -> Present
textToPresent str = (list!!0, list!!1, list!!2)
  where list = Data.List.map (\x -> read x :: Int) (Data.List.map unpack $ split ((=='x')) $ pack str)

shortestDistance :: OrderedPresent -> Int
shortestDistance present = (presentSide present 0)*2 + (presentSide present 1)*2
        
volumePresent :: OrderedPresent -> Int
volumePresent present = (presentSide present 0)*(presentSide present 1)*(presentSide present 2)
  
ribbonNeeded :: OrderedPresent -> Int
ribbonNeeded present = shortestDistance present + volumePresent present

-- MAIN FUNCTIONS

solve :: String -> (String, String)
solve input = (firstStar, secondStar)
  where lines' = Data.List.lines input
        presents = Data.List.map (presentToList . textToPresent) lines'
        firstStar = show $ sum $ Data.List.map paperNeeded presents
        secondStar = show $ sum $ Data.List.map ribbonNeeded presents

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput "2015" "2"
  let (firstStar, secondStar) = solve input
  -- print puzzle results
  printPuzzleResults firstStar secondStar
