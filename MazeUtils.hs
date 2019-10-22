module MazeUtils where

import Data.HashSet
import System.Random 

-- Utility module for Maze module

-- gets the first two elements as a pair from triplet
getPairFromTriplet :: (a, b, c) -> (a, b)
getPairFromTriplet (r, c, _) = (r, c)

-- get first element in a triplet
get1st :: (a, b, c) -> a
get1st (x,_,_) = x

-- get second element in a triplet
get2nd :: (a, b, c) -> b
get2nd (_,x,_) = x

-- get third element in a triplet
get3rd :: (a, b, c) -> c
get3rd (_, _, x) = x

-- creates an empty HashSet
-- More verbose than simply "empty"
createEmptySet :: HashSet a
createEmptySet = empty

-- Easier to write function to convert Int -> Num
num :: (Integral a, Num b) => a -> b
num = fromIntegral

-- Function to convert pair to ints to pair of Nums
pairIntToNum :: (Num b1, Num b2) => (Int, Int) -> (b1, b2)
pairIntToNum (x, y) = (num x, num y)

-- Given a random number generator
-- create a pair of random numbers
-- and return a triple having the next random number generator
-- as the third element
getRandomNumPair :: StdGen -> (Int, Int, StdGen)
getRandomNumPair generator = (firstRand, secondRand, nextGen)
   where firstGen = next generator
         firstRand = fst firstGen
         secondGen = next $ snd firstGen
         secondRand = fst secondGen
         nextGen = snd secondGen

-- Given a pair of random numbers, the function mods them with gridSize
modRandomNumPair :: Int -> (Int, Int) -> (Int, Int)
modRandomNumPair gridSize (r, c) = ((r `mod` gridSize) + 1, (c `mod` gridSize) + 1)

-- Given the size of the list, get a random index
getRandomIndex :: StdGen -> Int -> (Int, StdGen)
getRandomIndex generator size = (modded, nextGen)
   where (randomNum, nextGen) = next generator
         modded = randomNum `mod` size

-- Given a 0-based index i, remove element at that index
-- if index is out of bounds, return the list unchanged
removeElementAtIndex :: [a] -> Int -> [a]
removeElementAtIndex [] _ = []
removeElementAtIndex (first:list) 0 = list
removeElementAtIndex (first:list) i = first : removeElementAtIndex list (i - 1)

-- Find and remove element from list
removeElement :: (Eq a) => a -> [a] -> [a]
removeElement element = Prelude.foldr (\a b -> if a == element  then b else (a:b)) []

-- Add those elements of list a that dont exist in list b
unionList :: (Eq p) => [p] -> [p] -> [p]
unionList [] x = x
unionList x [] = x
unionList (first:listA) listB
   | first `elem` listB = unionList listA listB
   | otherwise = first : unionList listA listB

-- Remove duplicate walls from a grid
removeDuplicateWalls :: [(Integer, Integer, Char)] -> [(Integer, Integer, Char)]
removeDuplicateWalls [] = []
removeDuplicateWalls (wall : walls)
   | elem (getSecondRep wall) walls = removeDuplicateWalls walls
   | otherwise = wall : removeDuplicateWalls walls
   
-- given a non-edge wall, return its second representation
-- e.g wall (2, 3, 'L') is the same as wall (2, 2, 'R')
getSecondRep :: (Integer, Integer, Char) -> (Integer, Integer, Char)
getSecondRep (r, c, w)
   | w == 'L' = (r, c - 1, 'R')
   | w == 'R' = (r, c + 1, 'L')
   | w == 'U' = (r - 1, c, 'D')
   | w == 'D' = (r + 1, c, 'U')
   | otherwise = (-1, -1, 'X')