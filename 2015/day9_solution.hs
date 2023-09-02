import Data.List
import Data.Maybe
import IOHandler

type Location = String
type Distance = Int

type LocationList = [Location]
type DistancesLocation = [Distance]
type Distances = [DistancesLocation]
type Path = [Location]
type PathPairs = [(Location, Location)]

-- add value to list in position index
addToList :: [a] -> Int -> a -> [a]
addToList list index value = let (l1, l2) = splitAt index list in l1 ++ [value] ++ tail l2

-- Add location name to list of locations
addLocation :: LocationList -> Location -> LocationList
addLocation list location = if elem location list then list else list ++ [location]

-- Add distance to between two locations
addDistance :: Location -> Location -> LocationList -> Distances -> Distance -> Distances
addDistance location1 location2 locationList distances distance = distances''
  where index1 = fromJust $ elemIndex location1 locationList
        index2 = fromJust $ elemIndex location2 locationList
        line1 = distances !! index1
        line2 = distances !! index2
        line1' = addToList line1 index2 distance
        line2' = addToList line2 index1 distance
        distances' = addToList distances index1 line1'
        distances'' = addToList distances' index2 line2'

-- Add distances between pairs of locations
parseDistances :: (LocationList, Distances) -> String -> (LocationList, Distances)
parseDistances (locationList, distances) line = (locationList, addDistance location1 location2 locationList distances distance)
  where strings = words line
        location1 = strings !! 0
        location2 = strings !! 2
        distance = read (strings !! 4) :: Int

-- Add locations in input to location list
parseLocations :: LocationList -> String -> LocationList
parseLocations list line = locationList2
  where strings = words line
        firstLocation = strings !! 0
        secondLocation = strings !! 2
        locationList1 = addLocation list firstLocation
        locationList2 = addLocation locationList1 secondLocation

-- build every possible paths, permutation of locations
buildPaths :: LocationList -> [Path]
buildPaths = permutations

-- get distance between two locations
distanceLocations :: Location -> Location -> LocationList -> Distances -> Distance
distanceLocations location1 location2 locationList distances = (distances !! index1) !! index2
  where index1 = fromJust $ elemIndex location1 locationList
        index2 = fromJust $ elemIndex location2 locationList

-- calculate the total distance along the path
distancePath :: LocationList -> Distances -> PathPairs -> Distance
distancePath locationList distances pathPairs = foldl (\distance path -> distance + uncurry distanceLocations path locationList distances) 0 pathPairs

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput "2015" "9"
  let inputLines = lines input
  -- build location list with the list of different locations
  let locationList = foldl parseLocations [] inputLines
  -- build initial matrix of locations between pairs of locations, populated with 0
  let initialDistances = replicate (length locationList) $ replicate (length locationList) 0
  -- add values of distances between locations to matrix of locations
  let (_, distances) = foldl parseDistances (locationList, initialDistances) inputLines
  -- calculate all possible paths as a set of location pairs
  let paths = map (\path -> zip path $ tail path) $ buildPaths locationList
  -- calculate the distance for each path
  let distancePaths = map (distancePath locationList distances) paths
  let firstStar = minimum distancePaths
  let secondStar = maximum distancePaths
  -- print puzzle results
  printPuzzleResults firstStar secondStar
