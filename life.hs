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

type LogicGrid = [[Bool]]
type DisplayGrid = [[Char]]



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


-- Check for invalid characters in input file
-- TODO: also check for uneven row lengths
validateGrid :: DisplayGrid -> Bool
validateGrid  = and . map validateRow
  where
    validateRow = all valid
    valid cell = cell == alive || cell == dead



{- Retrieving cell values -}


top :: LogicGrid -> Int
top _ = 0

bottom :: LogicGrid -> Int
bottom grid = length grid - 1

leftEdge :: LogicGrid -> Int
leftEdge _ = 0

rightEdge :: LogicGrid -> Int
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
getAbove (x,y) grid | y == top grid = False
                    | otherwise = getCell (x, y-1) grid

getBelow :: Position -> LogicGrid -> Bool
getBelow (x,y) grid | y == bottom grid = False
                    | otherwise = getCell (x, y+1) grid

getLeft :: Position -> LogicGrid -> Bool
getLeft (x,y) grid | x == leftEdge grid = False
                   | otherwise = getCell (x-1, y) grid

getRight :: Position -> LogicGrid -> Bool
getRight (x,y) grid | x == rightEdge grid = False
                    | otherwise = getCell (x+1, y) grid

getAboveLeft :: Position -> LogicGrid -> Bool
getAboveLeft (x,y) grid | x == leftEdge grid = False
                        | y == top grid = False
                        | otherwise = getCell (x-1, y-1) grid

getAboveRight :: Position -> LogicGrid -> Bool
getAboveRight (x,y) grid | x == rightEdge grid = False
                         | y == top grid = False
                         | otherwise = getCell (x+1, y-1) grid

getBelowLeft :: Position -> LogicGrid -> Bool
getBelowLeft (x,y) grid | x == leftEdge grid = False
                        | y == bottom grid = False
                        | otherwise = getCell (x-1, y+1) grid

getBelowRight :: Position -> LogicGrid -> Bool
getBelowRight (x,y) grid | x == rightEdge grid = False
                         | y == bottom grid = False
                         | otherwise = getCell (x+1, y+1) grid



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
