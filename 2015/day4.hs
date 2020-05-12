import Data.Hash.MD5
import Data.List

main :: IO ()
main = do
  let str = "yzbqklnj"
  putStrLn $ "Calculating for puzzle input " ++ str ++ "..."
  let possibleKeys = map (\x -> (fst x ++ (show $ snd x), snd x)) $ zip (repeat str) [0..]
  let hashes = map (\x -> (md5s $ Str $ fst x, snd x)) possibleKeys
  let filteredHashes1 = filter (isPrefixOf "00000" . fst) hashes
  let filteredHashes2 = filter (isPrefixOf "000000" . fst) hashes
  putStrLn $ "The answer for the first star is " ++ (show $ snd $ filteredHashes1!!0)
  putStrLn $ "The answer for the second star is " ++ (show $ snd $ filteredHashes2!!0)
