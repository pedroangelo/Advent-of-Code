import Data.List (group, nub)
import Data.Char (ord, chr)

type Password = String

-- get the next char in the alphabeth, wrapping around in z
nextChar :: Char -> Char
nextChar char = if char == 'z' then 'a' else chr $ ord char + 1

-- generate next password
nextPassword :: Password -> Password
nextPassword password = reverse $ incrementPassword $ reverse password

-- generate the next valid password
nextValidPassword :: Password -> Password
nextValidPassword password = head $ dropWhile (not . validPassword) $ iterate nextPassword $ nextPassword password

-- increment password
incrementPassword :: Password -> Password
incrementPassword (p:password)
  | p == 'z' = 'a' : incrementPassword password
  | otherwise = nextChar p : password

-- text the validity of the password according to the 3 requirements
validPassword :: Password -> Bool
validPassword password = validPassword1 password && validPassword2 password && validPassword3 password

-- first requirement: include a increasing straight of 3 letters
validPassword1 :: Password -> Bool
validPassword1 (_:_:[]) = False
validPassword1 (p1:p2:p3:password)
  | (p1 == 'z') || (p2 == 'z') = validPassword1 $ p2:p3:password
  | (nextChar p1) == p2 && (nextChar p2) == p3 = True
  | otherwise = validPassword1 $ p2:p3:password

-- second requirement: do not contain i, o or j
validPassword2 :: Password -> Bool
validPassword2 password = and $ map (\char -> not $ any (\x -> char == x) ['i', 'o', 'l']) password

-- third requirement: contain at least 2 different pairs of letters
validPassword3 :: Password -> Bool
validPassword3 password = length pairs >= 2
  where pairs = nub $ filter (\x -> length x >= 2) $ group password

main :: IO ()
main = do
  putStr "Puzzle input: "
  puzzleInput <- getLine
  let firstStar = nextValidPassword puzzleInput
  putStrLn $ "First Star: " ++ (show firstStar)
  let secondStar = nextValidPassword firstStar
  putStrLn $ "Second Star: " ++ (show secondStar)
  
