import Data.List (transpose)
import IOHandler

type Name = String
type Capacity = Int
type Durability = Int
type Flavor = Int
type Texture = Int
type Calories = Int

-- (Name, [Capacity, Durability, Flavor, Texture, Calories])
type Ingredient = (Name, [Int])
type Amounts = [Int]
type Score = Int

-- parse line into ingredient
parseIngredient :: String -> Ingredient
parseIngredient line = (name, [capacity, durability, flavor, texture, calories])
  where strings = words line
        name = init $ strings !! 0
        capacity = read $ init $ strings !! 2
        durability = read $ init $ strings !! 4
        flavor = read $ init $ strings !! 6
        texture = read $ init $ strings !! 8
        calories = read  $ strings !! 10

-- calculate the score of the cookie
calculateCookieScore :: [Ingredient] -> Amounts -> Score
calculateCookieScore ingredients amounts = product $ propertyScore
  where
    -- pair of ammount and ingredient, represented by list of properties
    ammountIngredient = zip amounts $ map snd ingredients
    -- properties multipled by amount
    ammountProperties = map (\(p1,p2) -> map (p1*) (init p2)) $ ammountIngredient
    -- the score for all the properties
    propertyScore = map (max 0 . sum) $ transpose $ ammountProperties

-- generate distributions of total among number of iterations
generateAmounts :: Int -> Int -> [Amounts]
generateAmounts total 1 = [[total]]
generateAmounts total iteration = restAmount
  where firstAmount = [0..total]
        restAmount = concat $ map (\amount -> map (amount :) $ generateAmounts (total - amount) (iteration-1)) firstAmount

-- calculate highest score by calculating the maximum of all the combinations
calculateHighestScore :: [Ingredient] -> [Amounts] -> Score
calculateHighestScore ingredients amounts = maximum $ map (calculateCookieScore ingredients) amounts

-- filter amounts which correspond to cookies with 500 calories
filter500Calories :: [Ingredient] -> [Amounts] -> [Amounts]
filter500Calories ingredients amounts = filter (\amount -> (== 500) $ totalCalories amount) amounts
  where calories = map ((!!4) . snd) ingredients
        totalCalories amount = sum $ zipWith (*) amount calories

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput "2015" "15"
    -- parse ingredients
  let ingredients = map parseIngredient $ lines input
  -- generate distributions of different amounts of ingredients
  let amounts = generateAmounts 100 4
  let firstStar = calculateHighestScore ingredients amounts
  let secondStar = calculateHighestScore ingredients $ filter500Calories ingredients amounts
  -- print puzzle results
  printPuzzleResults firstStar secondStar
