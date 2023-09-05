module Event2015.Day6SolutionGeneric (main, solve) where

-- import Data.List.Split (splitOn)
import IOHandler

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end-start+1) . drop start

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

type Line a = [Light a]
type Grid a = [Line a]
type Light a = a
type Position = (Int, Int)
type Action a = Light a -> Light a
type Instruction a = (Action a, Position, Position)

data ActionSet a = ActionSet { turnOn :: Action a, turnOff :: Action a, toggle :: Action a }

turnOnF :: Action Bool
turnOnF _ = True

turnOffF :: Action Bool
turnOffF _ = False

toggleF :: Action Bool
toggleF = not

turnOnS :: Action Int
turnOnS = (+1)

turnOffS :: Action Int
turnOffS 0 = 0
turnOffS n = n - 1

toggleS :: Action Int
toggleS = (+2)

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

parseInstruction :: ActionSet a -> String -> Instruction a
parseInstruction actionSet string
  | head instruction == "toggle" = (toggle actionSet, posTopLeft, posBottomRight)
  | instruction!! 1 == "on" = (turnOn actionSet, posTopLeft, posBottomRight)
  | instruction!! 1 == "off" = (turnOff actionSet, posTopLeft, posBottomRight)
  where instruction = words string
        mod' = if head instruction == "toggle" then 0 else 1
        posTopLeft = (\x -> (read $ head x, read $ x!!1)) $ wordsWhen (==',') $ instruction !!(1+mod')
        posBottomRight = (\x -> (read $ head x, read $ x!!1)) $ wordsWhen (==',') $ instruction !!(3+mod')

buildGrid :: a -> Grid a
buildGrid elem = replicate 1000 $ replicate 1000 elem

-- MAIN FUNCTIONS

solve :: String -> (String, String)
solve input = (firstStar, secondStar)
  where actionSetF = ActionSet turnOnF turnOffF toggleF
        instructionsF = map (parseInstruction actionSetF) $ lines input
        gridF = foldl changeGrid (buildGrid False) instructionsF
        firstStar = show $ sum $ map (length . filter (True ==)) gridF

        actionSetS = ActionSet turnOnS turnOffS toggleS
        instructionsS = map (parseInstruction actionSetS) $ lines input
        gridS = foldl changeGrid (buildGrid 0) instructionsS
        secondStar = show $ sum $ map sum gridS

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput "2015" "6"
  let (firstStar, secondStar) = solve input
  -- print puzzle results
  printPuzzleResults firstStar secondStar
