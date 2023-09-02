import Data.Hash.MD5
import Data.List
import IOHandler

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput (PuzzleInfo "2015" "4")
  let possibleKeys = map (\x -> (fst x ++ (show $ snd x), snd x)) $ zip (repeat input) [0..]
  let hashes = map (\x -> (md5s $ Str $ fst x, snd x)) possibleKeys
  let filteredHashes1 = filter (isPrefixOf "00000" . fst) hashes
  let filteredHashes2 = filter (isPrefixOf "000000" . fst) hashes
  let firstStar = snd $ filteredHashes1!!0
  let secondStar = snd $ filteredHashes2!!0
    -- print puzzle results
  printPuzzleResults (PuzzleResult firstStar secondStar)
