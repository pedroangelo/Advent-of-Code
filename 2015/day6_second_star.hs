import Data.List.Split

type Line = [Light]
type Grid = [Line]
type Light = Int
type Position = (Int, Int)
type Action = Light -> Light
type Instruction = (Action, Position, Position)

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end-start+1) . drop start

turnOn :: Light -> Light
turnOn = (+1)

turnOff :: Light -> Light
turnOff 0 = 0
turnOff n = n - 1

toggle :: Light -> Light
toggle = (+2)

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
  where instruction = splitOn " " string
        mod' = if head instruction == "toggle" then 0 else 1
        posTopLeft = (\x -> (read $ head x, read $ x!!1)) $ splitOn "," $ instruction !!(1+mod')
        posBottomRight = (\x -> (read $ head x, read $ x!!1)) $ splitOn "," $ instruction !!(3+mod')

grid :: Grid
grid = replicate 1000 $ replicate 1000 0

main :: IO ()
main = do
  putStr "Filepath: "
  filePath <- getLine
  fileContents <- readFile filePath
  let instructions = map parseInstruction $ lines fileContents
  let grid' = foldl changeGrid grid instructions
  let lightsOn = sum $ map sum grid'
  putStrLn $ "Number of lit lights: " ++ show lightsOn
