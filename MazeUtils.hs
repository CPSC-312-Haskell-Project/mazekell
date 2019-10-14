module MazeUtils where

import Data.HashSet 

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

