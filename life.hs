import System.IO
import System.IO.Unsafe
import Control.Monad
import Control.Concurrent

{- Main game loop -}

_TIMESTEP = 1000000 -- microseconds

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  grid <- readGrid
  updateOutput grid

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

displayGrid :: LogicGrid -> DisplayGrid
displayGrid = map displayRow
  where displayRow = foldr (\x acc -> if x then alive:acc else dead:acc) []

interpretGrid :: DisplayGrid -> LogicGrid
interpretGrid = map interpretRow
  where interpretRow = foldr (\x acc -> (x==alive):acc) []

showGrid :: LogicGrid -> IO ()
showGrid = mapM_ putStrLn . displayGrid

{- File I/O -}

readGrid :: IO LogicGrid
readGrid = do
  handle <- openFile "initialState.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle
  return $ interpretGrid $ lines contents

-- TODO: error handling if e.g. file not found, grid not a rectangle

{- Retrieving cell values -}

top :: LogicGrid -> Int
top _ = 0

bottom :: LogicGrid -> Int
bottom grid = length grid - 1

leftEdge :: LogicGrid -> Int
leftEdge _ = 0

rightEdge :: LogicGrid -> Int
rightEdge grid = length (head grid) - 1

getCell :: Position -> LogicGrid -> Bool
getCell (x,y) grid = (grid !! y) !! x

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

liveNeighbours :: Position -> LogicGrid -> Int
liveNeighbours p = length . filter id . getNeighbours p

newCellValue :: Position -> LogicGrid -> Bool
newCellValue p grid = case getCell p grid of
  True -> case liveNeighbours p grid of
    0 -> False
    1 -> False
    2 -> True
    3 -> True
    x -> False
  False -> case liveNeighbours p grid of
    3 -> True
    x -> False

updateGrid :: LogicGrid -> LogicGrid
updateGrid grid = [updateRow y grid | y <- [top grid .. bottom grid]]
  where 
    updateRow y grid = [newCellValue (x,y) grid | x <- [leftEdge grid .. rightEdge grid]]
