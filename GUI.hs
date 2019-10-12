import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

width, height :: Int
width = 500
height = 500

window :: Display
window = InWindow "Maze Game" (width, height) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = rectangleSolid 50 50

data Game = Game
  { playerLoc :: (Float, Float),  -- player (x, y) location.
    goalLoc :: (Float, Float)  -- goal (x, y) location.
  } deriving Show 

renderMaze game =
   pictures [ translate    0   50  $ rectangleSolid 50 5
	    , translate    0 (-50) $ rectangleSolid 50 5
            , translate   50    0  $ rectangleSolid 5 50
            , translate (-50)   0  $ rectangleSolid 5 50
            , color red ball
            , color green goal
            ]
            where
            ball = uncurry translate (playerLoc game) (circle 10)
            goal = polygon [((x+10),  (y+10)), ((x+10),  (y-10)), ((x-10),  (y+10)), ((x-10),  (y-10))]
            (x, y) = goalLoc game

initialState = Game
  { playerLoc = (0, 0), -- startPoint, handle input from algorithm
    goalLoc = (100, 100) -- endPoint, handle input from algorithm
  }

handleInput (EventKey (Char 'w') _ _ _) game =
  game { playerLoc = (x, (y+10)), goalLoc = (goalLoc game) }
  where
    (x, y) = playerLoc game
handleInput (EventKey (Char 'a') _ _ _) game =
  game { playerLoc = ((x-10), y), goalLoc = (goalLoc game) }
  where
    (x, y) = playerLoc game
handleInput (EventKey (Char 's') _ _ _) game =
  game { playerLoc = (x, (y-10)), goalLoc = (goalLoc game) }
  where
    (x, y) = playerLoc game
handleInput (EventKey (Char 'd') _ _ _) game =
  game { playerLoc = ((x+10), y), goalLoc = (goalLoc game) }
  where
    (x, y) = playerLoc game
handleInput (EventKey (Char 'r') _ _ _) game =
  game { playerLoc = (0, 0), goalLoc = (goalLoc game) }
handleInput _ game = game

handleGoal game =
  if (x2-10) < x && x < (x2+10) && (y2-10) < y && y < (y2+10) then game { playerLoc = (0, 0), goalLoc = (goalLoc game) }
     else game
        where
           (x, y) = playerLoc game
           (x2, y2) = goalLoc game

updateState _ game = handleGoal game

main :: IO ()
main = display window background drawing

main2 = play window white 30 initialState renderMaze handleInput updateState

