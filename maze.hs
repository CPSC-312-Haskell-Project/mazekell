module Maze where

import Data.Char
import Data.HashSet
import System.Environment
import System.Exit
import System.Random

-- ALL IO AND RANDOM NUMBER LOGIC IS HERE

main :: IO()
main = do
   args <- getArgs
   verifyArgsOrQuit args
   seed <- getSeed args
   showSeed seed
   putStrLn "Done"

-- create a random generator with the specified seed value
getRandomGen :: Int -> StdGen
getRandomGen seed = mkStdGen seed

-- If a seed is specified, use it; otherwise, pick a random one
getSeed :: [String] -> IO Int
getSeed [] = getRandomSeed
getSeed (x:_) = return $ read x

-- Use the pre-seeded random generator to get a random seed
getRandomSeed :: IO Int
getRandomSeed = do
  randomSrc <- getStdGen
  return $ fst $ random $ randomSrc

-- To ouput seed to the user, so that the same maze can be generated again
showSeed :: Int -> IO ()
showSeed seed = putStrLn $ "The random seed is " ++ show seed

-- Argument verification code
verifyArgsOrQuit :: [String] -> IO ()
verifyArgsOrQuit args =
  if verifyArgs args
     then putStrLn "args ok!"
     else exitWithBadArgs

exitWithBadArgs :: IO ()
exitWithBadArgs = do 
  progName <- getProgName
  putStrLn $ "Use: " ++  progName ++ " [optional random seed]"
  exitWith $ ExitFailure 1

-- Legitimate arguments are none, or a string representing
-- a random seed.  Nothing else is accepted.
verifyArgs :: [String] -> Bool
verifyArgs [] = True
verifyArgs (x:xs) = Prelude.null xs && isNum x

-- Verify that input is a number.  This approach was chosen as read raises an
-- exception if it can't parse its input
isNum :: String -> Bool
isnum [] = False 
isNum (x:xs) = all isDigit xs && (x == '-' || isDigit x)


-- ALL MAZE LOGIC IS BELOW THIS

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
 
