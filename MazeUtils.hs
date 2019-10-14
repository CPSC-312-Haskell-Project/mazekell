module MazeUtils where

import Data.HashSet
import System.Random 

-- Utility module for Maze module

-- gets the first two elements as a pair from triplet
getPairFromTriplet (r, c, _) = (r, c)

-- get third element in a triplet
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

-- Merge lists
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys