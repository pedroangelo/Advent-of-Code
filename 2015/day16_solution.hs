import Data.Maybe (isNothing, fromMaybe)

type Possession = (String, Int)
type Sue = (Int, [Possession])

-- parse line into list of 
parseSue :: String -> Sue
parseSue line = (number, [(p1, q1), (p2, q2), (p3, q3)])
  where strings = words line
        number = read $ init $ strings !! 1
        p1 = init $ strings !! 2
        q1 = read $ init $ strings !! 3
        p2 = init $ strings !! 4
        q2 = read $ init $ strings !! 5
        p3 = init $ strings !! 6
        q3 = read  $ strings !! 7

-- verify if targetPossessions are compatible with suePossessions
sameSue :: Sue -> Sue -> Bool
sameSue (_, targetPossessions) (_, suePossessions) = and $ map (\tp -> samePossession tp suePossessions) targetPossessions

-- check if possession from list of things remembered (targetPossessions) possibly belongs to possessions
samePossession :: Possession -> [Possession] -> Bool
samePossession (targetPossession, targetQuantity) possessions
  | isNothing quantity = True
  | otherwise = targetQuantity == fromMaybe 1000 quantity
  where quantity = lookup targetPossession possessions

-- verify if targetPossessions are compatible with suePossessions
sameSue2 :: Sue -> Sue -> Bool
sameSue2 (_, targetPossessions) (_, suePossessions) = and $ map (\tp -> samePossession2 tp suePossessions) targetPossessions

-- check if possession from list of things remembered (targetPossessions) possibly belongs to possessions
samePossession2 :: Possession -> [Possession] -> Bool
samePossession2 (targetPossession, targetQuantity) possessions
  | isNothing quantity = True
  | otherwise =
    if targetPossession == "cats" || targetPossession == "trees" then targetQuantity < fromMaybe 1000 quantity else if targetPossession == "pomeranians" || targetPossession == "goldfish" then targetQuantity > fromMaybe 1000 quantity else targetQuantity == fromMaybe 1000 quantity
  where quantity = lookup targetPossession possessions

main :: IO ()
main = do
  putStrLn "Puzzle answer for day 16, event 2015!\n"
  putStr "Insert filepath: "
  filePath <- getLine
  fileContents <- readFile filePath
  -- parse list of compounds per sue
  let auntsSue = map parseSue $ lines fileContents
  let targetSue = (0, [("children",3), ("cats", 7), ("samoyeds", 2), ("pomeranians", 3), ("akitas", 0), ("vizslas", 0), ("goldfish", 5), ("trees",3), ("cars", 2), ("perfumes",1)])
  -- filter which sues are compatible by comparing their possessions
  let compatibleSue = filter (\sue -> sameSue targetSue sue) auntsSue
  let firstStar = fst $ head compatibleSue
  putStrLn $ "First star: " ++ show firstStar
  -- filter which sues are compatible by comparing their possessions
  let compatibleSue2 = filter (\sue -> sameSue2 targetSue sue) auntsSue
  let secondStar = fst $ head compatibleSue2
  putStrLn $ "Second star: " ++ show secondStar
