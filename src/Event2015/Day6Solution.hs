module Event2015.Day6Solution (main, solve) where

-- import Data.List.Split (splitOn)
import IOHandler

class Actionable a where
  turnOn :: Action a
  turnOff :: Action a
  toggle :: Action a

instance Actionable Bool where
  turnOn _ = True
  turnOff _ = False
  toggle = not

instance Actionable Int where
  turnOn = (+1)
  turnOff 0 = 0
  turnOff n = n - 1
  toggle = (+2)

instance Actionable Float where
  turnOn 1 = 1
  turnOn n = n + 0.1
  turnOff 0 = 0
  turnOff n = n - 0.1
  toggle 0 = 1
  toggle _ = 0

type Line a = [Light a]
type Grid a = [Line a]
type Light a = a
type Position = (Int, Int)
type Action a = Light a -> Light a
type Instruction a = (Action a, Position, Position)

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end-start+1) . drop start

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

changeGrid :: Grid a -> Instruction a -> Grid a
changeGrid grid (action, posTopLeft, posBottomRight) =
  -- leave unchanged the first lines of the grid
  topUnchanged ++ alteredGrid ++ bottomUnchanged
  where (x1,y1) = posTopLeft
        (x2,y2) = posBottomRight
        -- top lines of grid that don't get changed
        topUnchanged = take y1 grid
        -- bottom lines of grid that don't get changed
        bottomUnchanged = drop (y2+1) grid
        -- middle lines of grid that will get changed
        remainingGrid = slice y1 y2 grid
        -- altered lines of grid according to instruction
        alteredGrid = map (\line -> take x1 line ++ map action (slice x1 x2 line) ++ drop (x2+1) line) remainingGrid

parseInstruction :: Actionable a => String -> Instruction a
parseInstruction string
  | head instruction == "toggle" = (toggle, posTopLeft, posBottomRight)
  | instruction!! 1 == "on" = (turnOn, posTopLeft, posBottomRight)
  | instruction!! 1 == "off" = (turnOff, posTopLeft, posBottomRight)
  where instruction = words string
        mod' = if head instruction == "toggle" then 0 else 1
        posTopLeft = (\x -> (read $ head x, read $ x!!1)) $ wordsWhen (==',') $ instruction !!(1+mod')
        posBottomRight = (\x -> (read $ head x, read $ x!!1)) $ wordsWhen (==',') $ instruction !!(3+mod')

buildGrid :: a -> Grid a
buildGrid elem = replicate 1000 $ replicate 1000 elem

-- MAIN FUNCTIONS

solve :: String -> (String, String)
solve input = (firstStar, secondStar)
  where instructionsF = map (parseInstruction :: String -> Instruction Bool) $ lines input
        gridF = foldl changeGrid (buildGrid False) instructionsF
        firstStar = show $ sum $ map (length . filter (True ==)) gridF

        instructionsS = map (parseInstruction :: String -> Instruction Int) $ lines input
        gridS = foldl changeGrid (buildGrid 0) instructionsS
        secondStar = show $ sum $ map sum gridS

        instructionsT = map (parseInstruction :: String -> Instruction Float) $ lines input
        gridT = foldl changeGrid (buildGrid 0.0) instructionsT
        extraStar = show $ (/1000000) $ sum $ map sum gridT

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput "2015" "6"
  let (firstStar, secondStar) = solve input
  -- print puzzle results
  printPuzzleResults firstStar secondStar
