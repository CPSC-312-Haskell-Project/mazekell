module Maze where

import Data.HashSet

walls = ['L', 'R', 'U', 'D']

createGrid n = [(r, c, w) | r <- [1..n], c <- [1..n], w <- walls]
