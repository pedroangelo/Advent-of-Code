module Event2015.Day4Solution (main, solve) where

import Data.Hash.MD5
import Data.List
import IOHandler

-- MAIN FUNCTIONS

solve :: String -> (String, String)
solve input = (firstStar, secondStar)
  where possibleKeys = map (\x -> (fst x ++ (show $ snd x), snd x)) $ zip (repeat input) [0..]
        hashes = map (\x -> (md5s $ Str $ fst x, snd x)) possibleKeys
        filteredHashes1 = filter (isPrefixOf "00000" . fst) hashes
        firstStar = show $ snd $ filteredHashes1!!0
        filteredHashes2 = filter (isPrefixOf "000000" . fst) hashes
        secondStar = show $ snd $ filteredHashes2!!0

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput "2015" "4"
  let (firstStar, secondStar) = solve input
  -- print puzzle results
  printPuzzleResults firstStar secondStar
