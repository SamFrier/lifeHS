import System.IO
import System.Environment
import Control.Monad
import Control.Concurrent
import Data.Maybe

{- Main game loop -}


_TIMESTEP = 250000 -- microseconds


-- Entry point
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  if length args >= 1 then do
    grid <- readGrid $ head args
    if grid == Nothing then do
      error "Invalid input - please check the input file and try again"
      return ()
    else do
      updateOutput $ fromJust grid
  else do
    grid <- readGrid "default.txt"
    updateOutput $ fromJust grid


-- Repeatedly generate + display new grid state
updateOutput :: LogicGrid -> IO ()
updateOutput grid = do
  putStrLn ""
  threadDelay _TIMESTEP
  newGrid <- return $ updateGrid grid
  showGrid newGrid
  updateOutput newGrid

-- TODO: improve display of output



{- Type definitions -}


alive = '#'
dead = '.'

type PosX = Int
type PosY = Int
type Position = (PosX, PosY)
type CellValue = Char

type LogicGrid = [[Bool]]
type DisplayGrid = [[CellValue]]



{- Grid representation + display -}


-- Convert logical representation of grid to visual representation
displayGrid :: LogicGrid -> DisplayGrid
displayGrid = map displayRow
  where displayRow = foldr (\x acc -> if x then alive:acc else dead:acc) []


-- Convert contents of input file to logical representation
interpretGrid :: DisplayGrid -> LogicGrid
interpretGrid = map interpretRow
  where interpretRow = foldr (\x acc -> (x==alive):acc) []


-- Print grid to screen
showGrid :: LogicGrid -> IO ()
showGrid = mapM_ putStrLn . displayGrid



{- File I/O -}


-- Read in grid from file
readGrid :: FilePath -> IO (Maybe LogicGrid)
readGrid file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle
  if validateGrid $ lines contents then do
    return $ Just (interpretGrid $ lines contents)
  else do
    return Nothing


-- Check that an input grid is valid
validateGrid :: DisplayGrid -> Bool
validateGrid grid = validateRowLengths grid && validateChars grid


-- Check that all rows are the same (non-zero) length
validateRowLengths :: DisplayGrid -> Bool
validateRowLengths grid = firstRowLength /= 0 && all validRowLength grid
  where 
   validRowLength row = length row == firstRowLength
   firstRowLength = length $ head grid


-- Check for invalid characters in input file
validateChars :: DisplayGrid -> Bool
validateChars  = all validateRow
  where
    validateRow = all validChar
    validChar cell = cell == alive || cell == dead



{- Retrieving cell values -}


top :: LogicGrid -> PosY
top _ = 0

bottom :: LogicGrid -> PosY
bottom grid = length grid - 1

leftEdge :: LogicGrid -> PosX
leftEdge _ = 0

rightEdge :: LogicGrid -> PosX
rightEdge grid = length (head grid) - 1


-- Get value of a given cell
getCell :: Position -> LogicGrid -> Bool
getCell (x,y) grid = (grid !! y) !! x


-- Get values of a cell's eight neighbours
getNeighbours :: Position -> LogicGrid -> [Bool]
getNeighbours (x,y) grid = [a,b,l,r,al,ar,bl,br]
  where
    a = getAbove (x,y) grid
    b = getBelow (x,y) grid
    l = getLeft (x,y) grid
    r = getRight (x,y) grid
    al = getAboveLeft (x,y) grid
    ar = getAboveRight (x,y) grid
    bl = getBelowLeft (x,y) grid
    br = getBelowRight (x,y) grid

getAbove :: Position -> LogicGrid -> Bool
getAbove (x,y) grid = getCell (x, above) grid
  where 
    above = if y == top grid then bottom grid else y-1

getBelow :: Position -> LogicGrid -> Bool
getBelow (x,y) grid = getCell (x, below) grid
  where 
    below = if y == bottom grid then top grid else y+1

getLeft :: Position -> LogicGrid -> Bool
getLeft (x,y) grid = getCell (left, y) grid
  where
    left = if x == leftEdge grid then rightEdge grid else x-1

getRight :: Position -> LogicGrid -> Bool
getRight (x,y) grid = getCell (right, y) grid
  where
    right = if x == rightEdge grid then leftEdge grid else x+1

getAboveLeft :: Position -> LogicGrid -> Bool
getAboveLeft (x,y) grid = getCell (left, above) grid
  where
    left = if x == leftEdge grid then rightEdge grid else x-1
    above = if y == top grid then bottom grid else y-1

getAboveRight :: Position -> LogicGrid -> Bool
getAboveRight (x,y) grid = getCell (right, above) grid
  where
    right = if x == rightEdge grid then leftEdge grid else x+1
    above = if y == top grid then bottom grid else y-1

getBelowLeft :: Position -> LogicGrid -> Bool
getBelowLeft (x,y) grid = getCell (left, below) grid
  where
    left = if x == leftEdge grid then rightEdge grid else x-1
    below = if y == bottom grid then top grid else y+1 

getBelowRight :: Position -> LogicGrid -> Bool
getBelowRight (x,y) grid = getCell (right, below) grid
  where
    right = if x == rightEdge grid then leftEdge grid else x+1
    below = if y == bottom grid then top grid else y+1



{- Game logic -}


-- Determine number of neighbours that are alive
aliveNeighbours :: Position -> LogicGrid -> Int
aliveNeighbours p = length . filter id . getNeighbours p


-- Determine what a cell's upated value should be
newCellValue :: Position -> LogicGrid -> Bool
newCellValue p grid = case getCell p grid of
  True -> case aliveNeighbours p grid of
    2 -> True
    3 -> True
    x -> False
  False -> case aliveNeighbours p grid of
    3 -> True
    x -> False


-- Update the state of the entire grid
updateGrid :: LogicGrid -> LogicGrid
updateGrid grid = [updateRow y grid | y <- [top grid .. bottom grid]]
  where 
    updateRow y grid = [newCellValue (x,y) grid | x <- [leftEdge grid .. rightEdge grid]]

