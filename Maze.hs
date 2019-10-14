module Maze where

import MazeUtils
import Data.HashSet
import System.Random

-- This is the main function of this module
createMaze :: Int -> StdGen -> IO()
createMaze gridSize randomGen = do
   let firstRandomCellInt = modRandomNumPair gridSize $ getPairFromTriplet $ getRandomNumPair randomGen
   let firstRandomCell = pairIntToNum firstRandomCellInt
   let nextGenerator = get3rd $ getRandomNumPair randomGen
   let cellsSeen = singleton firstRandomCell
   let wallList = allCellWalls firstRandomCell
   let grid = createGrid gridSize
   let gridInteger = num gridSize
   let maze = primsAlgorithm wallList grid cellsSeen gridInteger nextGenerator
   putStrLn $ "Random Cell: " ++ (show firstRandomCell)
   putStrLn $ "Cells seen: " ++ (show cellsSeen)
   putStrLn $ "Maze: " ++ (show maze)

-- Run prim's algorithm
primsAlgorithm [] grid cellsSeen gridSize randomGenerator = cellsSeen
primsAlgorithm wallList grid cellsSeen gridSize randomGen = primsAlgorithm newWallList newGrid newCellsSeen gridSize newRandom
   where randomIndex = fst $ getRandomIndex randomGen $ length wallList
         newRandom = snd $ getRandomIndex randomGen $ length wallList
         wall = wallList !! randomIndex
         intermediateWallList = removeElementAtIndex wallList randomIndex
         isEdge = isEdgeWall gridSize wall
         otherWallRep = getSecondRep wall
         newWallList = if isEdge then intermediateWallList else removeElement otherWallRep intermediateWallList
         currCell = getCellFromWall wall
         otherCell = getCellFromWall otherWallRep
         thisSeen = member currCell cellsSeen
         otherSeen = member otherCell cellsSeen
         goodWall = (thisSeen && not (otherSeen)) || (not (thisSeen) && otherSeen)
         unseenCell = if thisSeen then otherCell else currCell
--         allUnseenCellWalls = allCellWalls unseenCell
--         newWallList = if goodWall then unionList allUnseenCellWalls newWallList1 else newWallList1
         newGrid1 = if goodWall then removeElement wall grid else grid
         newGrid = if goodWall then removeElement otherWallRep newGrid1 else newGrid1
         newCellsSeen1 = if goodWall then insert currCell cellsSeen else cellsSeen
         newCellsSeen = if goodWall then insert otherCell newCellsSeen1 else newCellsSeen1


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
getCellFromWall = getPairFromTriplet

