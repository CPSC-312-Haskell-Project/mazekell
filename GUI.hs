module GUI where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import MazeUtils

width, height :: Int
width = 900
height = 900

-- offset is for moving the entire maze origin to bottom left corner from center
offset :: Float
offset = 440

-- scale factor
c :: Float
c = 40

window :: Display
window = InWindow "Maze Game" (width, height) (100, 100)

background :: Color
background = white

drawing :: Picture
drawing = rectangleSolid 50 50

-- convert a triplet (a,b,l) to a Picture
triplet2Picture :: Float -> (Integer, Integer, Char) -> Picture
triplet2Picture h (a,b,l)
  | l == 'L' = translate (c*(fromIntegral(b)-1)-offset) (h-(c/2)-(c*(fromIntegral(a)-1))-offset)  $ rectangleSolid (c/10) c
  | l == 'R' = translate (c*fromIntegral(b)-offset) (h-(c/2)-(c*(fromIntegral(a)-1))-offset)  $ rectangleSolid (c/10) c
  | l == 'U' = translate (c*(fromIntegral(b)-1)+(c/2)-offset) (h-(c*(fromIntegral(a)-1))-offset)  $ rectangleSolid c (c/10)
  | l == 'D' = translate (c*(fromIntegral(b)-1)+(c/2)-offset) (h-(c*fromIntegral(a))-offset)  $ rectangleSolid c (c/10)
  | otherwise = Blank
  
-- calculate size
processSize :: Integer -> (Integer, Integer, Char) -> Picture
processSize size = triplet2Picture (c*fromIntegral(size))

-- convert maze walls array into picture array
walls2Pictures :: Integer -> [(Integer, Integer, Char)] -> [Picture]
walls2Pictures size = foldr (\x xs -> processSize size x : xs) []

data Game = Game
  { playerLoc :: (Float, Float),  -- player (x, y) location.
    goalLoc :: (Float, Float),  -- goal (x, y) location.
    wallList :: [(Integer, Integer, Char)], -- walls array
    size :: Integer
  } deriving Show 

renderMaze game =
   pictures (merge (walls2Pictures (size game) (wallList game)) [color red ball, color green goal])
            where
            ball = uncurry translate (playerLoc game) (circle 10)
            goal = polygon [((x+10),  (y+10)), ((x+10),  (y-10)), ((x-10),  (y+10)), ((x-10),  (y-10))]
            (x, y) = goalLoc game

initialState = Game
  { playerLoc = (0, 0), -- startPoint, handle input from algorithm
    goalLoc = (100, 100), -- endPoint, handle input from algorithm
    wallList = [(1,1,'L'),(1,1,'U'),(1,2,'U'),(1,3,'L'),(1,3,'R'),(1,3,'U'),(2,1,'L'),(2,2,'U'),(2,3,'R'),(3,1,'L'),(3,1,'D'),(3,2,'L'),(3,2,'D'),(3,3,'L'),(3,3,'R'),(3,3,'D')],
    size = 3
  }

handleInput (EventKey (Char 'w') _ _ _) game =
  game { playerLoc = (x, (y+20)), goalLoc = (goalLoc game), wallList = (wallList game), size = (size game)}
  where
    (x, y) = playerLoc game
handleInput (EventKey (Char 'a') _ _ _) game =
  game { playerLoc = ((x-20), y), goalLoc = (goalLoc game), wallList = (wallList game), size = (size game) }
  where
    (x, y) = playerLoc game
handleInput (EventKey (Char 's') _ _ _) game =
  game { playerLoc = (x, (y-20)), goalLoc = (goalLoc game), wallList = (wallList game), size = (size game) }
  where
    (x, y) = playerLoc game
handleInput (EventKey (Char 'd') _ _ _) game =
  game { playerLoc = ((x+20), y), goalLoc = (goalLoc game), wallList = (wallList game), size = (size game) }
  where
    (x, y) = playerLoc game
handleInput (EventKey (Char 'r') _ _ _) game =
  game { playerLoc = (0, 0), goalLoc = (goalLoc game), wallList = (wallList game), size = (size game) }
handleInput _ game = game

handleGoal game =
  if (x2-20) < x && x < (x2+20) && (y2-20) < y && y < (y2+20) then game { playerLoc = (0, 0), goalLoc = (goalLoc game), wallList = (wallList game), size = (size game) }
     else game
        where
           (x, y) = playerLoc game
           (x2, y2) = goalLoc game

updateState _ game = handleGoal game

--main :: IO ()
--main = display window background drawing
main = play window background 30 initialState renderMaze handleInput updateState

-- make GUI takes in a list of walls and produces a game
makeGUI w gs = play window background 30 Game
  { playerLoc = (0, 0), -- startPoint, handle input from algorithm
    goalLoc = (100, 100), -- endPoint, handle input from algorithm
    wallList = w,
    size = gs
  } renderMaze handleInput updateState
  