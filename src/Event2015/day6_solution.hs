-- import Data.List.Split (splitOn)
import IOHandler

type Line = [Light]
type Grid = [Line]
type Light = Bool
type Position = (Int, Int)
type Action = Light -> Light
type Instruction = (Action, Position, Position)

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end-start+1) . drop start

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

turnOn :: Light -> Light
turnOn _ = True

turnOff :: Light -> Light
turnOff _ = False

toggle :: Light -> Light
toggle = not

changeGrid :: Grid -> Instruction -> Grid
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

parseInstruction :: String -> Instruction
parseInstruction string
  | head instruction == "toggle" = (toggle, posTopLeft, posBottomRight)
  | instruction!! 1 == "on" = (turnOn, posTopLeft, posBottomRight)
  | instruction!! 1 == "off" = (turnOff, posTopLeft, posBottomRight)
  where instruction = words string
        mod' = if head instruction == "toggle" then 0 else 1
        posTopLeft = (\x -> (read $ head x, read $ x!!1)) $ wordsWhen (==',') $ instruction !!(1+mod')
        posBottomRight = (\x -> (read $ head x, read $ x!!1)) $ wordsWhen (==',') $ instruction !!(3+mod')

grid :: Grid
grid = replicate 1000 $ replicate 1000 False

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput "2015" "6"
  let instructions = map parseInstruction $ lines input
  let grid' = foldl changeGrid grid instructions
  let firstStar = sum $ map (length . filter (True ==)) grid'
  let secondStar = "unfinished"
  -- print puzzle results
  printPuzzleResults firstStar secondStar
