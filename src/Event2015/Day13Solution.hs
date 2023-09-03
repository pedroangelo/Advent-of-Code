module Event2015.Day13Solution (main, solve) where

import Data.Maybe (fromJust)
import Data.List (elemIndex, permutations, replicate)
import IOHandler

type Person = String
type Happiness = Int

type People = [Person]
type PotentialHappinessByPerson = [Happiness]
type PotentialHappiness = [PotentialHappinessByPerson]

type Neighbors = [(Person, Person)]

-- add value to list in position index
addToList :: [a] -> Int -> a -> [a]
addToList list index value = let (l1, l2) = splitAt index list in l1 ++ [value] ++ tail l2

-- Add person to list of people
addPerson :: People -> Person -> People
addPerson people person = if elem person people then people else people ++ [person]

-- Add person in input to people list
parsePerson :: People -> String -> People
parsePerson people line = people2
  where strings = words line
        firstPerson = strings !! 0
        secondPerson = init $ strings !! 10
        people1 = addPerson people firstPerson
        people2 = addPerson people1 secondPerson

-- Add relative happiness between pairs of people
parsePotentialHappiness :: (People, PotentialHappiness) -> String -> (People, PotentialHappiness)
parsePotentialHappiness (people, potentialHappiness) line = (people, potentialHappiness')
  where strings = words line
        firstPerson = strings !! 0
        firstPersonIndex = fromJust $ elemIndex firstPerson people
        firstPersonPotentialHappiness = potentialHappiness !! firstPersonIndex
        secondPerson = init $ strings !! 10
        secondPersonIndex = fromJust $ elemIndex secondPerson people
        happiness = if (strings !! 2) == "lose" then (-1) * read (strings !! 3) else 1 * read (strings !! 3)
        firstPersonPotentialHappiness' = addToList firstPersonPotentialHappiness secondPersonIndex happiness
        potentialHappiness' = addToList potentialHappiness firstPersonIndex firstPersonPotentialHappiness'

-- build every possible neighbors combination
buildNeighborsCombinations :: People -> [Neighbors]
buildNeighborsCombinations people = map (\seatingArrangement -> zip seatingArrangement (tail seatingArrangement) ++ [(last seatingArrangement, head seatingArrangement)]) $ permutations people

-- get relative happiness between two neighbors
relativeHappinessNeighbors :: Person -> Person -> People -> PotentialHappiness -> Happiness
relativeHappinessNeighbors person1 person2 people potentialHappiness = happinessPerson1 + happinessPerson2
  where index1 = fromJust $ elemIndex person1 people
        index2 = fromJust $ elemIndex person2 people
        happinessPerson1 = (potentialHappiness !! index1) !! index2
        happinessPerson2 = (potentialHappiness !! index2) !! index1

-- calculate the total happiness along the seating arrangement
totalHappiness :: People -> PotentialHappiness -> Neighbors -> Happiness
totalHappiness people potentialHappiness neighbors = foldl (\happiness seatingArrangement -> happiness + uncurry relativeHappinessNeighbors seatingArrangement people potentialHappiness) 0 neighbors

-- MAIN FUNCTIONS

solve :: String -> (String, String)
solve input = (firstStar, secondStar)
  where inputLines = lines input
        -- build list of different people
        people = foldl parsePerson [] inputLines
        -- build initial matrix of potential happiness between pairs of people, populated with 0
        initialPotentialHappiness = replicate (length people) $ replicate (length people) 0
        -- add happiness between people to matrix of potential happiness
        (_, potentialHappiness) = foldl parsePotentialHappiness (people, initialPotentialHappiness) inputLines
        -- calculate all possible seating arrangements as a set of neighbors
        neighborsCombinations = buildNeighborsCombinations people
        -- calculate the total happiness for each seating arrangement
        totalHappinessCombinations = map (totalHappiness people potentialHappiness) neighborsCombinations
        firstStar = show $ maximum totalHappinessCombinations
        
        -- build list of people with me
        peopleMe = people ++ ["Me"]
        -- build matrix of potential happiness with me
        potentialHappinessMe = map (\potentialHappinessByPerson -> potentialHappinessByPerson ++ [0]) potentialHappiness ++ [replicate (length peopleMe) 0]
        -- calculate all possible seating arrangements with me
        neighborsCombinationsMe = buildNeighborsCombinations peopleMe
        -- calculate the total happiness for each seating arrangement with me
        totalHappinessCombinationsMe = map (totalHappiness peopleMe potentialHappinessMe) neighborsCombinationsMe
        secondStar = show $ maximum totalHappinessCombinationsMe

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput "2015" "13"
  let (firstStar, secondStar) = solve input
  -- print puzzle results
  printPuzzleResults firstStar secondStar
