module MazeCli where

import Data.Char
import System.Environment
import System.Exit
import System.Random
import Maze

-- ALL IO AND RANDOM NUMBER LOGIC IS HERE

main :: IO()
main = do
   args <- getArgs
   verifyArgsOrQuit args
   seed <- getSeed args
   showSeed seed
   putStrLn "Please enter the grid size"
   mazeSize <- getNum "\n Please enter a valid number:"
   createMaze  mazeSize (getRandomGen  seed)

-- Prompt until a valid number is read, and return it
getNum :: String -> IO Int
getNum prompt =
  getFromStdin prompt getLine isNum read

-- This contains the logic common to getNum;
-- it repeatedly prompts until input matching some criteria
-- is given, transforms that input, and returns it
getFromStdin :: String -> (IO a) -> (a -> Bool) -> (a -> b) -> IO b
getFromStdin prompt inputF isOk transformOk = do
  input <- inputF
  if isOk input
     then return $ transformOk input
     else do
       putStr prompt
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


