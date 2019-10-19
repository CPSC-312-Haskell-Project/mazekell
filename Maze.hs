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
getMaze gridSize randomGen = primsAlgorithm wallList grid cellsSeen gridSize nextGenerator
   where firstRandomCellInt = modRandomNumPair  (num gridSize) $ getPairFromTriplet $ getRandomNumPair randomGen
         firstRandomCell = pairIntToNum firstRandomCellInt
         nextGenerator = get3rd $ getRandomNumPair randomGen
         cellsSeen = singleton firstRandomCell
         wallList = allCellWalls firstRandomCell
         grid = fromList $ createGrid gridSize


-- Run prim's algorithm
primsAlgorithm [] grid cellsSeen gridSize randomGenerator = (grid, randomGenerator)
primsAlgorithm wallList grid cellsSeen gridSize randomGen = primsAlgorithm newWallList newGrid newCellsSeen gridSize newRandom
   where randomIndex = fst $ getRandomIndex randomGen $ length wallList
         newRandom = snd $ getRandomIndex randomGen $ length wallList
         wall = wallList !! randomIndex
         intermediateWallList = removeElementAtIndex wallList randomIndex
         otherWallRep = getSecondRep wall
         newWallList1 = removeElement otherWallRep intermediateWallList
         otherCell = getCellFromWall otherWallRep
         isValidCell = cellInRange otherCell gridSize
         otherSeen = member otherCell cellsSeen
         allUnseenCellWalls = allCellWalls otherCell
         unseenWalls = removeElement otherWallRep allUnseenCellWalls
         proceed = isValidCell && not otherSeen
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

