module Maze where

import MazeUtils
import Data.HashSet
import System.Random

-- This is the main function of this module
createMaze :: Int -> StdGen -> ([(Integer, Integer, Char)], Integer, StdGen)
createMaze gridSize randomGen = (maze, gridInteger, nextGen)
   where gridInteger = num gridSize
         mazeRand = getMaze gridInteger randomGen
         maze = toList $ fst $ mazeRand
         nextGen = snd $ mazeRand
   -- putStrLn $ "Original grid wall list length: " ++ (show $ length maze)


-- Top level wrapper for running Prim's Algorithm, that can be called to get a maze
getMaze :: Integer -> StdGen -> (HashSet (Integer, Integer, Char), StdGen)
getMaze gridSize randomGen = primsAlgorithm wallList grid cellsSeen gridSize nextGenerator
   where firstRandomCellInt = modRandomNumPair  (num gridSize) $ getPairFromTriplet $ getRandomNumPair randomGen
         firstRandomCell = pairIntToNum firstRandomCellInt
         nextGenerator = get3rd $ getRandomNumPair randomGen
         cellsSeen = singleton firstRandomCell
         wallList = allCellWalls firstRandomCell
         grid = fromList $ createGrid gridSize


{--
This algorithm is a modified version of Prim's algorithm.

1. Start with a list of all walls.
2. Pick a cell, mark it as part of the maze. Add the walls of the cell to the wall list.
3. While there are walls in the list:
      Pick a random wall from the list. If only one of the two cells that the wall divides is visited, then:
         - Make the wall a passage and mark the unvisited cell as part of the maze.
         - Add the neighboring walls of the cell to the wall list.
      Remove the wall from the list.
--}

primsAlgorithm :: [(Integer, Integer, Char)]
                  -> HashSet (Integer, Integer, Char)
                  -> HashSet (Integer, Integer)
                  -> Integer
                  -> StdGen
                  -> (HashSet (Integer, Integer, Char), StdGen)
primsAlgorithm [] grid cellsSeen gridSize randomGenerator = (grid, randomGenerator)
primsAlgorithm wallList grid cellsSeen gridSize randomGen = primsAlgorithm newWallList newGrid newCellsSeen gridSize newRandom
   where randomIndex = fst $ getRandomIndex randomGen $ length wallList
         newRandom = snd $ getRandomIndex randomGen $ length wallList
         wall = wallList !! randomIndex                                                       -- pick a wall
         intermediateWallList = removeElementAtIndex wallList randomIndex                     -- remove the wall and create a new list
         otherWallRep = getSecondRep wall                                                     -- a wall has two representations, find it
         newWallList1 = removeElement otherWallRep intermediateWallList                       -- remove second representation of same wall
         otherCell = getCellFromWall otherWallRep                                             -- get the second cell with the same wall
         isValidCell = cellInRange otherCell gridSize                                         -- make sure it is not out of bounds
         otherSeen = member otherCell cellsSeen                                               -- is the second cell already visited?
         allUnseenCellWalls = allCellWalls otherCell                                          -- get the walls of the second cell
         unseenWalls = removeElement otherWallRep allUnseenCellWalls                          -- remove the shared wall
         proceed = isValidCell && not otherSeen                                               -- proceed only if the second cell is valid and not seen
         newWallList = if proceed then unionList unseenWalls newWallList1 else newWallList1
         newGrid1 = if proceed then delete wall grid else grid
         newGrid = if proceed then delete otherWallRep newGrid1 else newGrid1
         newCellsSeen = if proceed then insert otherCell cellsSeen else cellsSeen

-- wall representation in our maze. The left wall, right wall, upper wall and lower wall
-- are represented as 'L', 'R', etc. respectively
walls :: [Char]
walls = ['L', 'R', 'U', 'D']

-- given a cell, get all its walls
allCellWalls :: (Integer, Integer) -> [(Integer, Integer, Char)]
allCellWalls (r, c) = [(r, c, w) | w <- walls]

-- Creates a grid list full of walls
createGrid :: (Integral a, Num b, Num c) => a -> [(b, c, Char)]
createGrid gridSize = [(num r, num c, w) | r <- [1..gridSize], c <- [1..gridSize], w <- walls]

-- Checks if a cell is in range of the grid
cellInRange :: (Integer, Integer) -> Integer -> Bool
cellInRange (r, c) gridSize = r > 0 && r <= gridSize && c > 0 && c <= gridSize

-- given gridsize and a wall, returns true if the wall is an edge wall
isEdgeWall :: Integer -> (Integer, Integer, Char) -> Bool
isEdgeWall gridSize (r, c, w)
   | (w == 'U' && r == 1) = True
   | (w == 'D' && r == gridSize) = True
   | (w == 'L' && c == 1) = True
   | (w == 'R' && c == gridSize) = True
   | otherwise = False

-- given a wall, return the cell it belongs to
getCellFromWall :: (Integer, Integer, Char) -> (Integer, Integer)
getCellFromWall = getPairFromTriplet

