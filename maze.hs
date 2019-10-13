module Maze where

import Data.HashSet
import System.Random

-- createMaze :: Int -> StdGen -> Int
createMaze randomGen = next randomGen

-- creates an empty HashSet
-- More verbose than simply "empty"
createEmptySet :: HashSet a
createEmptySet = empty

-- wall representation in our maze. The left wall, right wall, upper wall and lower wall
-- are represented as 'L', 'R', etc. respectively
walls :: [Char]
walls = ['L', 'R', 'U', 'D']

-- Easier to write function to convert Int -> Num
num :: (Integral a, Num b) => a -> b
num = fromIntegral 

-- Creates a grid list full of walls
createGrid :: (Integral a, Num b, Num c) => a -> [(b, c, Char)]
createGrid gridSize = [(num r, num c, w) | r <- [1..gridSize], c <- [1..gridSize], w <- walls]

-- generates a random grid cell
-- randomCell gridSize = (num (randomNumber 1 gridSize), num (randomNumber 1 gridSize))

-- given gridsize and a wall, returns true if the wall is an edge wall
isEdgeWall :: Integer -> (Integer, Integer, Char) -> Bool
isEdgeWall gridSize (r, c, w)
   | (w == 'U' && r == 1) = True
   | (w == 'D' && r == gridSize) = True
   | (w == 'L' && c == 1) = True
   | (w == 'R' && c == gridSize) = True
   | otherwise = False
 
-- given a non-edge wall, return its second representation
-- e.g wall (2, 3, 'L') is the same as wall (2, 2, 'R')
getSecondRep :: (Integer, Integer, Char) -> (Integer, Integer, Char)
getSecondRep (r, c, w)
   | w == 'L' = (r, c - 1, 'R')
   | w == 'R' = (r, c + 1, 'L')
   | w == 'U' = (r - 1, c, 'D')
   | w == 'D' = (r + 1, c, 'U')
   | otherwise = (-1, -1, 'X')

-- given a wall, return the cell it belongs to
getCellFromWall :: (Integer, Integer, Char) -> (Integer, Integer)
getCellFromWall (r, c, w) = (r, c)
 
