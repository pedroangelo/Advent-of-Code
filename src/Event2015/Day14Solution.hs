module Event2015.Day14Solution (main, solve) where

import Data.List (transpose)
import IOHandler

type Speed = Int
type Duration = Int
type Rest = Int
type Time = Int
type Distance = Int

type Reindeer = (Speed, Duration, Rest)

-- calculate distance traveled by reindeer
calculateDistanceTravelled :: Time -> Reindeer -> Distance
calculateDistanceTravelled time (speed, duration, rest) = distanceUntilRest + distanceAfterRest
  -- calculate the distance traveled until the end of the final rest
  where distanceUntilRest = (*) (speed * duration) $ div time (duration + rest)
  -- calculate the distance traveled during the final burst and final rest
        distanceAfterRest = if mod time (duration + rest) >= duration then (speed * duration) else (mod time (duration + rest)) * speed

-- calculate winners by time
calculateWinnerTime :: Time -> [Reindeer] -> [Bool]
-- calculate distance travelled for each reindeer at a certain time; then award the maximum ones
calculateWinnerTime time reindeers = (\distances -> map (maximum distances ==) distances) $ map (\reindeer -> calculateDistanceTravelled time reindeer) reindeers

-- parse line into reindeer
parseReindeer :: String -> Reindeer
parseReindeer line = (speed, duration, rest)
  where strings = words line
        speed = read $ strings !! 3
        duration = read $ strings !! 6
        rest = read $ strings !! 13

-- MAIN FUNCTIONS

solve :: String -> (String, String)
solve input = (firstStar, secondStar)
  where
        -- get list of reindeer
        reindeers = map parseReindeer $ lines input
        -- calculate the reindeer that travelled the most after 2503 seconds
        firstStar = show $ maximum $ map (calculateDistanceTravelled 2503) reindeers
        -- calculate which reindeer accumulated more victory points by being first in each second
        secondStar = show $ maximum $ map (length . filter id) $ transpose $ map (\time -> calculateWinnerTime time reindeers) [1..2503]

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput "2015" "14"
  let (firstStar, secondStar) = solve input
  -- print puzzle results
  printPuzzleResults firstStar secondStar
