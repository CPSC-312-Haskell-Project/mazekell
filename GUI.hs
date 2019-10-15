module GUI where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import MazeUtils

width, height :: Int
width = 800
height = 800

window :: Display
window = InWindow "Maze Game" (width, height) (100, 100)

wall :: Float -> Float -> Picture
wall w h = color black (rectangleSolid w h)

goalPic :: Float -> Picture
goalPic r = color green (circleSolid r)

playerPic :: Float -> Picture
playerPic r = color red (circleSolid r)

-- offset is for moving the entire maze origin to bottom left corner from center
offset :: Float
offset = 390

-- scale factor
scaleFactor :: Float -> Float
scaleFactor size = (fromIntegral height-20) / size

data Game = Game
  { playerLoc :: (Float, Float),  -- player (x, y) location.
    goalLoc :: (Float, Float),  -- goal (x, y) location.
    mazeWalls :: [(Float, Float, Char)],  -- wall (x,y,dir) locations.
    mwd :: [(Integer, Integer, Char)],
    size :: Float -- grid size
  } deriving Show 

initialState :: [(Float, Float, Char)] -> [(Integer, Integer, Char)] -> Float -> Game
initialState maze mwd size = Game
  { playerLoc = (0-o+(c/2), 0-o+(c/2)), -- startPoint, handle input from algorithm
    goalLoc = (0+o-(c/2), 0+o-(c/2)), -- endPoint, handle input from algorithm
    mazeWalls = maze,
    mwd = mwd,
    size = size
  }
     where
     c = scaleFactor size
     o = offset

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
   | dir == 'U' = translate (c*(y-1)+(c/2)-(offset)) (h-(c*(x-1))-(offset)) $ wall c (c/10)
   | dir == 'D' = translate (c*(y-1)+(c/2)-(offset)) (h-(c*x)-(offset))  $ rectangleSolid c (c/10)
   | dir == 'L' = translate (c*(y-1)-(offset)) (h-(c/2)-(c*(x-1))-(offset))  $ rectangleSolid (c/10) c
   | dir == 'R' = translate (c*y-(offset)) (h-(c/2)-(c*(x-1))-(offset))  $ rectangleSolid (c/10) c
      where
         x = get1st t
         y = get2nd t
         dir = get3rd t
         c = scaleFactor size
         h = c * size

handleInput :: Event -> Game -> Game
handleInput (EventKey (Char 'w') Down _ _) game
   | checkMove game 'U' = game { playerLoc = (x, (y+unit)), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game), mwd = (mwd game), size = (size game) }
   | otherwise = game
      where
         (x, y) = playerLoc game
         unit = scaleFactor (size game)
         
handleInput (EventKey (Char 'a') Down _ _) game
   | checkMove game 'L' = game { playerLoc = ((x-unit), y), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game), mwd = (mwd game), size = (size game) }
   | otherwise = game
      where
         (x, y) = playerLoc game
         unit = scaleFactor (size game)
         
handleInput (EventKey (Char 's') Down _ _) game
   | checkMove game 'D' = Game { playerLoc = (x, (y-unit)), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game), mwd = (mwd game), size = (size game) }
   | otherwise = game
      where
         (x, y) = playerLoc game
         unit = scaleFactor (size game)
                  
handleInput (EventKey (Char 'd') Down _ _) game
   | checkMove game 'R' = Game { playerLoc = ((x+unit),y), goalLoc = (goalLoc game), mazeWalls = (mazeWalls game), mwd = (mwd game), size = (size game) }
   | otherwise = game
      where
         (x, y) = playerLoc game
         unit = scaleFactor (size game)

handleInput (EventKey (Char 'r') Down _ _) game =
   initialState (mazeWalls game) (mwd game) (size game)

handleInput _ game =
   game

handleGoal :: Game -> Game
handleGoal game =
  if (x2-unit) < x && x < (x2+unit) && (y2-unit) < y && y < (y2+unit) then initialState (mazeWalls game) (mwd game) (size game) -- This resets the game state
     else game
        where
           (x, y) = playerLoc game
           (x2, y2) = goalLoc game
           unit = (scaleFactor (size game)) / 4

updateState :: Float -> Game -> Game
updateState _ game =
   handleGoal game

createGUI :: Integral a => [(Integer, Integer, Char)] -> [(Integer, Integer, Char)] -> a -> IO ()
createGUI maze mwd size = do
   let mazeWalls = map (parseMazeWall) maze
   let sizeFloat = num size
   let initState = initialState mazeWalls mwd sizeFloat
   play window white 30 initState renderMaze handleInput updateState

-- converts coordinates to triplet
findGridLocation :: Float -> (Float, Float) -> Char -> (Integer, Integer, Char)
findGridLocation s (x,y) dir = 
   (toInteger (ceiling (s-(fromIntegral (div (toInteger (ceiling (mul*(o+y)))) c)))), 1+(div (toInteger (ceiling (mul*(o+x)))) c), dir)
      where
      mul = 100000
      o = offset
      c = toInteger (ceiling (mul*(scaleFactor s)))
      
      
-- finds triplet in list
findWall :: (Integer, Integer, Char) -> [(Integer, Integer, Char)] -> Bool
findWall (x,y,l) list = foldr (\ (j,k,m) r -> ((j==x) && (k==y) && (m==l)) || r) False list

-- helper to simplifly handleInput:
checkMove :: Game -> Char -> Bool
checkMove game dir = not (findWall (findGridLocation (size game) (playerLoc game) dir) (mwd game))
