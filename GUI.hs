module GUI where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import MazeUtils

width, height :: Int
width = 1000
height = 1000

window :: Display
window = InWindow "Maze Game" (width, height) (100, 100)

wall :: Float -> Float -> Picture
wall w h = color black (rectangleSolid w h)

goalPic :: Picture
goalPic = color green (rectangleSolid 30 30)

playerPic :: Picture
playerPic = color red (circle 10)

-- offset is for moving the entire maze origin to bottom left corner from center
offset :: Float
offset = 440

-- scale factor
c :: Float
c = 40

data Game = Game
  { playerLoc :: (Float, Float),  -- player (x, y) location.
    goalLoc :: (Float, Float),  -- goal (x, y) location.
    mazeWalls :: [(Float, Float, Char)],  -- wall (x,y,dir) locations.
    size :: Integer -- grid size
  } deriving Show 

initialState :: [(Float, Float, Char)] -> Integer -> Game
initialState maze size = Game
  { playerLoc = (0, 0), -- startPoint, handle input from algorithm
    goalLoc = (100, 100), -- endPoint, handle input from algorithm
    mazeWalls = maze,
    size = size
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
         player = uncurry translate (playerLoc game) playerPic
         walls = pictures (createWalls (mazeWalls game) (size game))
         goal = uncurry translate (goalLoc game) goalPic

createWalls :: [(Float, Float, Char)] -> Integer -> [Picture]
createWalls mazeWalls size =
   map (parseWall size) mazeWalls

parseWall :: Integer -> (Float, Float, Char) -> Picture
parseWall size t
   | dir == 'U' = translate (c*(y-1)+(c/2)-offset) (h-(c*(x-1))-offset) $ wall c (c/10)
   | dir == 'D' = translate (c*(y-1)+(c/2)-offset) (h-(c*x)-offset)  $ rectangleSolid c (c/10)
   | dir == 'L' = translate (c*(y-1)-offset) (h-(c/2)-(c*(x-1))-offset)  $ rectangleSolid (c/10) c
   | dir == 'R' = translate (c*y-offset) (h-(c/2)-(c*(x-1))-offset)  $ rectangleSolid (c/10) c
      where
         x = get1st t
         y = get2nd t
         dir = get3rd t
         h = c*fromIntegral(size)

handleInput :: Event -> Game -> Game
handleInput (EventKey (Char 'w') _ _ _) game =
   game { playerLoc = (x, (y+10)), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game), size = (size game) }
   where
      (x, y) = playerLoc game
handleInput (EventKey (Char 'a') _ _ _) game =
   game { playerLoc = ((x-10), y), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game), size = (size game) }
   where
      (x, y) = playerLoc game
handleInput (EventKey (Char 's') _ _ _) game =
    game { playerLoc = (x, (y-10)), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game), size = (size game) }
    where
       (x, y) = playerLoc game
handleInput (EventKey (Char 'd') _ _ _) game =
   game { playerLoc = ((x+10), y), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game), size = (size game) }
   where
      (x, y) = playerLoc game
handleInput (EventKey (Char 'r') _ _ _) game =
   game { playerLoc = (0, 0), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game), size = (size game) }
handleInput _ game =
   game

handleGoal :: Game -> Game
handleGoal game =
  if (x2-20) < x && x < (x2+20) && (y2-20) < y && y < (y2+20) then game { playerLoc = (0, 0), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game), size = (size game) } -- This resets the game state
     else game
        where
           (x, y) = playerLoc game
           (x2, y2) = goalLoc game

updateState :: Float -> Game -> Game
updateState _ game =
   handleGoal game

createGUI maze size = do
   -- Some Issue here
   let mazeWalls = map (parseMazeWall) maze
   let initState = initialState mazeWalls size
   play window white 30 initState renderMaze handleInput updateState

