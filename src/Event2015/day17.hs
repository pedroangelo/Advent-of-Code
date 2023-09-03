import Data.Sort (sortBy)
  
type Container = Int
type Liters = Int
type Combination = [Container]

-- generate combinations which exactly fit liters
generateCombinations :: Liters -> Combination -> Combination -> [Combination]
-- when reaching the end of the list of containers, if the chosen containers so far hold exactly liters, then return them otherwise return empty list
generateCombinations liters chosenContainers [] = if sum chosenContainers == liters then [chosenContainers] else []
-- divide combinations into two: keeping and not keeping the head of the list of containers
generateCombinations liters chosenContainers (container:remainingContainers) = combinationHead ++ combinationTail
  where
    -- combinations obtained from keeping the head of the list of containers
    combinationHead
      -- if containers hold more than liters, discard combination
      | sum newCombination > liters = []
      -- if containers hold exactly liters, return combination
      | sum newCombination == liters = [newCombination]
      -- if containers still do not hold liters, keep adding containers
      | otherwise = generateCombinations liters newCombination remainingContainers
    -- combinations obtained from not keeping the head of the list of containers
    combinationTail = generateCombinations liters chosenContainers remainingContainers
    newCombination = chosenContainers ++ [container]

main :: IO ()
main = do
  putStr "Filepath: "
  filePath <- getLine
  fileContents <- readFile filePath
  -- parse list of compounds per sue
  let containers = sortBy (flip compare) (map (\x -> read x :: Container) $ lines fileContents)
  -- different combinations that hold exactly 150 liters
  let combinations = generateCombinations 150 [] containers
  putStrLn $ "First star: " ++ show (length combinations)
  -- least amount of containers to for a combination that holds exactly 150 liters
  let least = minimum $ map length combinations
  -- different combinations using the minimum number of containers
  putStrLn $ "Second star: " ++ show (length $ filter (\c -> length c == least) combinations)
