module Event2015.Day18Solution (main, solve) where

import IOHandler

data Mode = FirstStar | SecondStar deriving Eq
type Light = Bool
type Line = [Light]
type Grid = [Line]
type XPos = Int
type YPos = Int
type LightPos = (XPos, YPos)

-- parse grid line
parseLine :: String -> Line
parseLine [] = []
parseLine (l:line)
  | l == '#' = True : parseLine line
  | l == '.' = False : parseLine line

-- calculate how many light are on
calculateOnLights :: Grid -> Int
calculateOnLights = sum . map (length . filter id)

-- get specific light status given the x and y coordinate
getLight :: Grid -> LightPos -> Light
getLight grid (xpos, ypos) = grid !! ypos !! xpos

-- get adjacent coordinates
adjacentCoords :: LightPos -> [LightPos]
adjacentCoords (xpos, ypos) = [tl, t, tr, l, r, bl, b, br]
  where tl = (xpos-1,ypos-1)
        t = (xpos, ypos-1)
        tr = (xpos+1, ypos-1)
        l = (xpos-1, ypos)
        r = (xpos+1, ypos)
        bl = (xpos-1, ypos+1)
        b = (xpos, ypos+1)
        br = (xpos+1, ypos+1)

-- get the list of neighbors given a single light
getNeighbors :: Grid -> LightPos -> [Light]
getNeighbors grid lightPos = map (getLight grid) neighbors
  where heightGrid = length grid
        widthGrid = length $ head grid
        neighbors = filter (\(x,y) -> (x >= 0 && x < widthGrid) && (y >= 0 && y < heightGrid)) $ adjacentCoords lightPos

-- evolve light in a given grid
evolveLight :: Mode -> Grid -> LightPos -> Light
evolveLight mode grid lightPos
  -- second star
  | mode == SecondStar && (lightPos == (0,0) || lightPos == (0,heightGrid-1) || lightPos == (widthGrid-1,0) || lightPos == (widthGrid-1,heightGrid-1)) = True
  -- a light which is on stays on when 2 or 3 neighbors are on
  | getLight grid lightPos && (onNeighbors == 2 || onNeighbors == 3) = True
  -- a light which is off turns on if exactly 3 neighbors are on
  | (not $ getLight grid lightPos) && onNeighbors == 3 = True
  -- otherwise turn or stay off
  | otherwise = False
  where onNeighbors = length $ filter id $ getNeighbors grid lightPos
        heightGrid = length grid
        widthGrid = length $ head grid

-- evolve grid by one step
evolveGrid :: Mode -> Grid -> Grid
evolveGrid mode grid = map (map (evolveLight mode grid)) positions
  where heightGrid = length grid
        widthGrid = length $ head grid
        positions = [[(x, y) | x <- [0..widthGrid-1]] | y <- [0..heightGrid-1]]

-- MAIN FUNCTIONS

solve :: String -> (String, String)
solve input = (firstStar, secondStar)
  where
        -- let ingredients = map parseIngredient $ lines fileContents
        grid = map parseLine $ lines input
        -- calculate how many lights are on after 100 iterations
        firstStar = show $ calculateOnLights $ iterate (evolveGrid FirstStar) grid !! 100
        -- fix lights in the corner as on
        grid' = (True : (init $ tail $ head grid) ++ [True]) : (init $ tail $ grid) ++ [(True : (init $ tail $ last grid) ++ [True])]
        -- recalculate, with the corner light stuck in on
        secondStar = show $ calculateOnLights $ iterate (evolveGrid SecondStar) grid' !! 100

main :: IO ()
main = do
  -- print puzzle info and get input from user
  input <- obtainPuzzleInput "2015" "18"
  let (firstStar, secondStar) = solve input
  -- print puzzle results
  printPuzzleResults firstStar secondStar
