module Maze where

import Data.HashSet
import System.Random
import System.IO.Unsafe (unsafePerformIO)

-- wall representation in our maze. The left wall, right wall, upper wall and lower wall
-- are represented as 'L', 'R', etc. respectively
walls = ['L', 'R', 'U', 'D']

-- Easier to write function to convert Int -> Num
num n = fromIntegral n

-- Creates a grid list full of walls
createGrid gridSize = [(num r, num c, w) | r <- [1..gridSize], c <- [1..gridSize], w <- walls]

-- creates an empty HashSet
-- More verbose than simply "empty"
createEmptySet = empty

-- Generates a random number of the for IO a
randomNumberIO lowerBound upperBound = randomRIO (lowerBound, upperBound)

-- Converts IO a -> a for easier operations on the random number generated
randomNumber lowerBound upperBound = unsafePerformIO (randomNumberIO lowerBound upperBound)

-- generates a random grid cell
randomCell gridSize = (num (randomNumber 1 gridSize), num (randomNumber 1 gridSize)) 
