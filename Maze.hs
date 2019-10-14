module Maze where

import MazeUtils
import Data.HashSet
import System.Random

-- This is the main function of this module
createMaze :: Int -> StdGen -> IO()
createMaze gridSize randomGen = do
   let randomNum = ((fst $ next randomGen) `mod` gridSize) + 1
   let firstRandom = getRandomNumPair randomGen
   putStrLn $ "The first random pair is: " ++ (show $ getPairFromTriplet firstRandom)
   let secondGen = get3rd firstRandom
   putStrLn $ "The next random pair is: " ++ (show $ getRandomNumPair secondGen)

getRandomNumPair :: StdGen -> (Int, Int, StdGen)
getRandomNumPair generator = (firstRand, secondRand, nextGen)
   where firstGen = next generator
         firstRand = fst firstGen
         secondGen = next $ snd firstGen
         secondRand = fst secondGen
         nextGen = snd secondGen

-- wall representation in our maze. The left wall, right wall, upper wall and lower wall
-- are represented as 'L', 'R', etc. respectively
walls :: [Char]
walls = ['L', 'R', 'U', 'D']

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
 
