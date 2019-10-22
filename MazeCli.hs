module MazeCli where

import Data.Char
import System.Environment
import System.Exit
import System.Random
import Maze
import MazeUtils
import GUI

-- This module contains all CLI logic 

-- The main function calls createMaze and passes it to createGUI
main :: IO()
main = do
   args <- getArgs
   verifyArgsOrQuit args
   seed <- getSeed args
   putStrLn "---"
   putStrLn "Welcome to Mazekell!"
   putStrLn "---"
   putStrLn "Controls:"
   putStrLn "Use WASD or Arrow Keys to make your way to the end of the maze."
   putStrLn "Press R to reset your position to the start of the maze."
   putStrLn "Press Escape to close the game."
   putStrLn "---"
   putStrLn "Game Settings:"
   showSeed seed
   putStrLn "Please enter the grid size (2 <= x <= 50):"
   mazeSize <- getNum "Please enter a valid integer (2 <= x <= 50):"
   let (maze, gridInteger, nextGen) = createMaze mazeSize (getRandomGen seed)
   createGUI maze gridInteger nextGen

-- Prompt until a valid number is read, and return it
getNum :: String -> IO Int
getNum prompt =
  getFromStdin prompt getLine validateGridSize read

-- This contains the logic common to getNum;
-- it repeatedly prompts until input matching some criteria
-- is given, transforms that input, and returns it
getFromStdin :: String -> (IO a) -> (a -> Bool) -> (a -> b) -> IO b
getFromStdin prompt inputF isOk transformOk = do
  input <- inputF
  if isOk input
     then return $ transformOk input
     else do
       putStrLn prompt
       getFromStdin prompt inputF isOk transformOk

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
showSeed seed = putStrLn $ "The random seed is " ++ show seed ++ "."

-- Argument verification code
verifyArgsOrQuit :: [String] -> IO ()
verifyArgsOrQuit args =
  if verifyArgs args
     then putStrLn "args ok!"
     else exitWithBadArgs

-- Show message and exit if the argument is invalid
exitWithBadArgs :: IO ()
exitWithBadArgs = do 
  progName <- getProgName
  putStrLn $ "Arg must be an Integer."
  putStrLn $ "Use: " ++  progName ++ " [optional seed]"
  exitWith $ ExitFailure 1

-- Legitimate arguments are none, or a string representing
-- a random seed.  Nothing else is accepted.
verifyArgs :: [String] -> Bool
verifyArgs [] = True
verifyArgs [['-']] = False
verifyArgs (x:xs) = Prelude.null xs && isInt x

-- Validate gridSize which the user inputs
-- If the user does not input a string, or the value of the integer
-- is > 50 or < 2, then return false
validateGridSize :: String -> Bool
validateGridSize [] = False
validateGridSize ['-'] = False
validateGridSize input = isInt input && (read input <= 50) && (read input >= 2)

-- Verify that input is an Int.  This approach was chosen as read raises an
-- exception if it can't parse its input
isInt :: String -> Bool
isInt [] = False
isInt (x:xs) = all isDigit xs && (x == '-' || isDigit x)


