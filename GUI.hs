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

goalPic :: Float -> Picture
goalPic r = color green (circleSolid r)

playerPic :: Float -> Picture
playerPic r = color red (circleSolid r)

-- offset is for moving the entire maze origin to bottom left corner from center
offset :: Float -> Float
offset size
     | size < 5 = size * size * (50 / size)
     | size < 10 = size * size * (40 / size)
     | size < 20 = size * size * (20 / size)
     | size < 30 = size * size * (15 / size)
     | size < 40 = size * size * (10 / size)
     | size < 50 = size * size * (5 / size)

-- scale factor
scaleFactor :: Float -> Float
scaleFactor size
  | size < 5 = size * (100 / size)
  | size < 10 = size * (80 / size)
  | size < 20 = size * (40 / size)
  | size < 30 = size * (30 / size)
  | size < 40 = size * (20 / size)
  | size < 50 = size * (10 / size)

data Game = Game
  { playerLoc :: (Float, Float),  -- player (x, y) location.
    goalLoc :: (Float, Float),  -- goal (x, y) location.
    mazeWalls :: [(Float, Float, Char)],  -- wall (x,y,dir) locations.
    size :: Float -- grid size
  } deriving Show 

initialState :: [(Float, Float, Char)] -> Float -> Game
initialState maze size = Game
  { playerLoc = (0 - (offset size) + (c/4), 0 - (offset size) + (c/4)), -- startPoint, handle input from algorithm
    goalLoc = (0 + (offset size) - (c/4), 0 + (offset size) - (c/4)), -- endPoint, handle input from algorithm
    mazeWalls = maze,
    size = size
  }
     where
     c = scaleFactor size

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
         c = scaleFactor (size game)
         player = uncurry translate (playerLoc game) (playerPic (c/4))
         walls = pictures (createWalls (mazeWalls game) (size game))
         goal = uncurry translate (goalLoc game) (goalPic (c/4))

createWalls :: [(Float, Float, Char)] -> Float -> [Picture]
createWalls mazeWalls size =
   map (parseWall size) mazeWalls

parseWall :: Float -> (Float, Float, Char) -> Picture
parseWall size t
   | dir == 'U' = translate (c*(y-1)+(c/2)-(offset size)) (h-(c*(x-1))-(offset size)) $ wall c (c/10)
   | dir == 'D' = translate (c*(y-1)+(c/2)-(offset size)) (h-(c*x)-(offset size))  $ rectangleSolid c (c/10)
   | dir == 'L' = translate (c*(y-1)-(offset size)) (h-(c/2)-(c*(x-1))-(offset size))  $ rectangleSolid (c/10) c
   | dir == 'R' = translate (c*y-(offset size)) (h-(c/2)-(c*(x-1))-(offset size))  $ rectangleSolid (c/10) c
      where
         x = get1st t
         y = get2nd t
         dir = get3rd t
         c = scaleFactor size
         h = c * size

handleInput :: Event -> Game -> Game
handleInput (EventKey (Char 'w') _ _ _) game =
   game { playerLoc = (x, (y+unit)), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game), size = (size game) }
   where
      (x, y) = playerLoc game
      unit = (scaleFactor (size game)) / 4
handleInput (EventKey (Char 'a') _ _ _) game =
   game { playerLoc = ((x-unit), y), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game), size = (size game) }
   where
      (x, y) = playerLoc game
      unit = (scaleFactor (size game)) / 4
handleInput (EventKey (Char 's') _ _ _) game =
    game { playerLoc = (x, (y-unit)), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game), size = (size game) }
    where
       (x, y) = playerLoc game
       unit = (scaleFactor (size game)) / 4
handleInput (EventKey (Char 'd') _ _ _) game =
   game { playerLoc = ((x+unit), y), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game), size = (size game) }
   where
      (x, y) = playerLoc game
      unit = (scaleFactor (size game)) / 4
handleInput (EventKey (Char 'r') _ _ _) game =
   initialState (mazeWalls game) (size game)
handleInput _ game =
   game

handleGoal :: Game -> Game
handleGoal game =
  if (x2-unit) < x && x < (x2+unit) && (y2-unit) < y && y < (y2+unit) then initialState (mazeWalls game) (size game) -- This resets the game state
     else game
        where
           (x, y) = playerLoc game
           (x2, y2) = goalLoc game
           unit = (scaleFactor (size game)) / 4

updateState :: Float -> Game -> Game
updateState _ game =
   handleGoal game

createGUI maze size = do
   let mazeWalls = map (parseMazeWall) maze
   let sizeFloat = num size
   let initState = initialState mazeWalls sizeFloat
   play window white 30 initState renderMaze handleInput updateState
