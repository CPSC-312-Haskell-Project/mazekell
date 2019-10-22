module GUI where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import MazeUtils
import Maze

-- width and height of the window
width, height :: Int
width = 800
height = 800

-- window properties
window :: Display
window = InWindow "Maze Game" (width, height) (100, 100)

-- a single unit wall of the maze
wall :: Float -> Float -> Picture
wall w h = color black (rectangleSolid w h)

-- the green dot that depicts the goal 
goalPic :: Float -> Picture
goalPic r = color green (circleSolid r)

-- the red dot that depicts the player 
playerPic :: Float -> Picture
playerPic r = color red (circleSolid r)

-- offset is for moving the entire maze origin to bottom left corner from center
offset :: Float
offset = (num height-20) / 2

-- scale factor, i.e. the width/height of an individual box on the grid
scaleFactor :: Float -> Float
scaleFactor size = (num height-20) / size

-- Game defines the state of a game at any point
data Game = Game
  { playerLoc :: (Float, Float),  -- player (x, y) location.
    goalLoc :: (Float, Float),  -- goal (x, y) location.
    mazeWalls :: [(Float, Float, Char)],  -- wall (x,y,dir) locations.
    mwd :: [(Integer, Integer, Char)], -- maze walls with duplicates, used in wall avoidance
    size :: Float, -- grid size
    nextGen :: StdGen
  } deriving Show 

-- initialState creates the starting state of the maze
-- Places player at bottom left corner and end goal at top right
initialState :: [(Float, Float, Char)] -> [(Integer, Integer, Char)] -> Float -> StdGen -> Game
initialState maze mwd size nextGen = Game
  { playerLoc = (0-o+(c/2), 0-o+(c/2)), -- startPoint, handle input from algorithm
    goalLoc = (0+o-(c/2), 0+o-(c/2)), -- endPoint, handle input from algorithm
    mazeWalls = maze,
    mwd = mwd,
    size = size,
    nextGen = nextGen
  }
     where
     c = scaleFactor size
     o = offset

-- renders maze
renderMaze :: Game -> Picture
renderMaze game =
   pictures [player, goal, walls]
      where
         c = scaleFactor (size game)
         player = uncurry translate (playerLoc game) (playerPic (c/4))
         walls = pictures (createWalls (mazeWalls game) (size game))
         goal = uncurry translate (goalLoc game) (goalPic (c/4))

-- convert walls to list of Pictures for gloss to render
createWalls :: [(Float, Float, Char)] -> Float -> [Picture]
createWalls mazeWalls size =
   map (parseWall size) mazeWalls
  
-- utility func to convert integer wall locations to floats 
parseMazeWall :: (Integer, Integer, Char) -> (Float, Float, Char)
parseMazeWall (x,y,dir) = (num (x), num (y), dir)

-- places a wall in its correct place on the window coordinate frame
parseWall :: Float -> (Float, Float, Char) -> Picture
parseWall size (x,y,dir)
   | dir == 'U' = translate (c*(y-1)+(c/2)-(offset)) (h-(c*(x-1))-(offset)) $ wall c (c/10)
   | dir == 'D' = translate (c*(y-1)+(c/2)-(offset)) (h-(c*x)-(offset))  $ rectangleSolid c (c/10)
   | dir == 'L' = translate (c*(y-1)-(offset)) (h-(c/2)-(c*(x-1))-(offset))  $ rectangleSolid (c/10) c
   | dir == 'R' = translate (c*y-(offset)) (h-(c/2)-(c*(x-1))-(offset))  $ rectangleSolid (c/10) c
      where
         c = scaleFactor size
         h = c * size

-- keyboard input for game control
-- handleInput consumes a key event and a game state, and returns the updated game state
handleInput :: Event -> Game -> Game
handleInput (EventKey (Char 'w') Down _ _) game
   | checkMove game 'U' = Game {
      playerLoc = (x, (y+unit)),
      goalLoc = (goalLoc game),
      mazeWalls = (mazeWalls game),
      mwd = (mwd game),
      size = (size game),
      nextGen = (nextGen game) }
   | otherwise = game
      where
         (x, y) = playerLoc game
         unit = scaleFactor (size game)

handleInput (EventKey (SpecialKey KeyUp) Down _ _) game
   | checkMove game 'U' = Game {
      playerLoc = (x, (y+unit)),
      goalLoc = (goalLoc game),
      mazeWalls = (mazeWalls game),
      mwd = (mwd game),
      size = (size game),
      nextGen = (nextGen game) }
   | otherwise = game
      where
         (x, y) = playerLoc game
         unit = scaleFactor (size game)
         
handleInput (EventKey (Char 'a') Down _ _) game
   | checkMove game 'L' = Game {
      playerLoc = ((x-unit), y), 
      goalLoc = (goalLoc game), 
      mazeWalls = (mazeWalls game), 
      mwd = (mwd game), 
      size = (size game),
      nextGen = (nextGen game) }
   | otherwise = game
      where
         (x, y) = playerLoc game
         unit = scaleFactor (size game)

handleInput (EventKey (SpecialKey KeyLeft) Down _ _) game
   | checkMove game 'L' = Game {
      playerLoc = ((x-unit), y),
      goalLoc = (goalLoc game),
      mazeWalls = (mazeWalls game),
      mwd = (mwd game),
      size = (size game),
      nextGen = (nextGen game) }
   | otherwise = game
      where
         (x, y) = playerLoc game
         unit = scaleFactor (size game)
         
handleInput (EventKey (Char 's') Down _ _) game
   | checkMove game 'D' = Game {
      playerLoc = (x, (y-unit)),
      goalLoc = (goalLoc game),
      mazeWalls = (mazeWalls game),
      mwd = (mwd game),
      size = (size game),
      nextGen = (nextGen game) }
   | otherwise = game
      where
         (x, y) = playerLoc game
         unit = scaleFactor (size game)

handleInput (EventKey (SpecialKey KeyDown) Down _ _) game
   | checkMove game 'D' = Game {
      playerLoc = (x, (y-unit)),
      goalLoc = (goalLoc game),
      mazeWalls = (mazeWalls game),
      mwd = (mwd game),
      size = (size game),
      nextGen = (nextGen game) }
   | otherwise = game
      where
         (x, y) = playerLoc game
         unit = scaleFactor (size game)

handleInput (EventKey (Char 'd') Down _ _) game
   | checkMove game 'R' = Game {
      playerLoc = ((x+unit),y), 
      goalLoc = (goalLoc game), 
      mazeWalls = (mazeWalls game), 
      mwd = (mwd game), 
      size = (size game),
      nextGen = (nextGen game) }
   | otherwise = game
      where
         (x, y) = playerLoc game
         unit = scaleFactor (size game)

handleInput (EventKey (SpecialKey KeyRight) Down _ _) game
   | checkMove game 'R' = Game {
      playerLoc = ((x+unit),y),
      goalLoc = (goalLoc game),
      mazeWalls = (mazeWalls game),
      mwd = (mwd game),
      size = (size game),
      nextGen = (nextGen game) }
   | otherwise = game
      where
         (x, y) = playerLoc game
         unit = scaleFactor (size game)

handleInput (EventKey (Char 'r') Down _ _) game =
   initialState (mazeWalls game) (mwd game) (size game) (nextGen game)

handleInput _ game = game

-- when the goal is reached, create a new maze with a different seed
handleGoal :: Game -> Game
handleGoal game =
  if (x2-unit) < x && x < (x2+unit) && (y2-unit) < y && y < (y2+unit) then newGame -- This resets the game state
     else game
        where
           (x, y) = playerLoc game
           (x2, y2) = goalLoc game
           unit = (scaleFactor (size game)) / 4
           (newMaze, newGridInteger, newNextGen) = createMaze (round (size game)) (nextGen game)
           newMazeWalls = map (parseMazeWall) (removeDuplicateWalls newMaze)
           newSizeFloat = num newGridInteger
           newGame = initialState newMazeWalls newMaze newSizeFloat newNextGen

-- gloss update state
updateState :: Float -> Game -> Game
updateState _ game = handleGoal game

-- sets initial state and launches the game window
createGUI :: Integral a => [(Integer, Integer, Char)] -> a -> StdGen -> IO ()
createGUI maze size nextGen = do
   let mazeWalls = map (parseMazeWall) (removeDuplicateWalls maze)
   let sizeFloat = num size
   let initState = initialState mazeWalls maze sizeFloat nextGen
   play window white 30 initState renderMaze handleInput updateState

-- helper to simplifly handleInput: search if move is valid by checking surrounding walls
checkMove :: Game -> Char -> Bool
checkMove game dir = not (findWall (findGridLocation (size game) (playerLoc game) dir) (mwd game))

-- converts screen coordinates to triplet representation of the wall
findGridLocation :: Float -> (Float, Float) -> Char -> (Integer, Integer, Char)
findGridLocation s (x,y) dir = 
   (toInteger (ceiling (s-(num (div (toInteger (ceiling (mul*(o+y)))) c)))), 1+(div (toInteger (ceiling (mul*(o+x)))) c), dir)
      where
      mul = 100000
      o = offset
      c = toInteger (ceiling (mul*(scaleFactor s)))
      
-- finds triplet in list
findWall :: (Integer, Integer, Char) -> [(Integer, Integer, Char)] -> Bool
findWall (x,y,l) list = foldr (\ (j,k,m) r -> ((j==x) && (k==y) && (m==l)) || r) False list
