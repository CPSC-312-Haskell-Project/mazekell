module GUI where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import MazeUtils

width, height :: Int
width = 700
height = 700

window :: Display
window = InWindow "Maze Game" (width, height) (10, 10)

wall :: Picture
wall = color black (rectangleSolid 5 10)

goal :: Picture
goal = color green (rectangleSolid 10 10)

player :: Picture
player = color red (circle 10)

data Game = Game
  { playerLoc :: (Float, Float),  -- player (x, y) location.
    goalLoc :: (Float, Float),  -- goal (x, y) location.
    mazeWalls :: [(Float, Float, Char)]  -- wall (x,y,dir) locations.
  } deriving Show 

initialState :: [(Float, Float, Char)] -> Game
initialState maze = Game
  { playerLoc = (0, 0), -- startPoint, handle input from algorithm
    goalLoc = (100, 100), -- endPoint, handle input from algorithm
    mazeWalls = maze
  }

parseMazeWall :: (Integer, Integer, Char) -> (Float, Float, Char)
parseMazeWall t =
   (fromIntegral(x), fromIntegral(y), dir)
   where
         x = get1st t
         y = get2nd t
         dir = get3rd t

renderMaze :: Game -> Picture
renderMaze game =
   pictures [player, goal, walls]
      where
         player = uncurry translate (playerLoc game) player
         walls = pictures (createWalls (mazeWalls game))
         goal = uncurry translate (goalLoc game) goal


createWalls :: [(Float, Float, Char)] -> [Picture]
createWalls mazeWalls =
   map (parseWall) mazeWalls

parseWall :: (Float, Float, Char) -> Picture
parseWall t =
   rotate degree (translate ((x) * 10) ((y) * 10)  $ wall)
   where
      x = get1st t
      y = get2nd t
      degree = getDegree (get3rd t)

getDegree :: Char -> Float
getDegree dir
   | dir == 'U' = 0
   | dir == 'D' = 90
   | dir == 'L' = 180
   | dir == 'R' = 270
   | otherwise = 0

handleInput :: Event -> Game -> Game
handleInput (EventKey (Char 'w') _ _ _) game =
   game { playerLoc = (x, (y+10)), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game) }
   where
      (x, y) = playerLoc game
handleInput (EventKey (Char 'a') _ _ _) game =
   game { playerLoc = ((x-10), y), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game) }
   where
      (x, y) = playerLoc game
handleInput (EventKey (Char 's') _ _ _) game =
    game { playerLoc = (x, (y-10)), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game) }
    where
       (x, y) = playerLoc game
handleInput (EventKey (Char 'd') _ _ _) game =
   game { playerLoc = ((x+10), y), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game) }
   where
      (x, y) = playerLoc game
handleInput (EventKey (Char 'r') _ _ _) game =
   game { playerLoc = (0, 0), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game) }
handleInput _ game =
   game

handleGoal :: Game -> Game
handleGoal game =
  if (x2-10) < x && x < (x2+10) && (y2-10) < y && y < (y2+10) then game { playerLoc = (0, 0), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game) } -- This resets the game state
     else game
        where
           (x, y) = playerLoc game
           (x2, y2) = goalLoc game

updateState :: Float -> Game -> Game
updateState _ game =
   handleGoal game

createGUI maze = do
   -- Some Issue here
   let mazeWalls = map (parseMazeWall) maze
   let initState = initialState mazeWalls
   play window white 30 initState renderMaze handleInput updateState

