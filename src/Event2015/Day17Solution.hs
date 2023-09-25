module Event2015.Day17Solution (main, solve) where

import IOHandler
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

solve :: String -> (String, String)
solve input = (firstStar, secondStar)
  where containers = sortBy (flip compare) (map (\x -> read x :: Container) $ lines input)
        combinations = generateCombinations 150 [] containers
        firstStar = show $ length combinations
        
        least = minimum $ map length combinations
        secondStar = show (length $ filter (\c -> length c == least) combinations)

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput "2015" "17"
  let (firstStar, secondStar) = solve input
  -- print puzzle results
  printPuzzleResults firstStar secondStar
