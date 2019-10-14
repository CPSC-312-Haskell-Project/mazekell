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
    walls :: [(Integer, Integer, Char)], -- walls array
    size :: Integer
  } deriving Show 

renderMaze game =
   pictures (merge (walls2Pictures (size game) (walls game)) [color red ball, color green goal])
            where
            ball = uncurry translate (playerLoc game) (circle 10)
            goal = polygon [((x+10),  (y+10)), ((x+10),  (y-10)), ((x-10),  (y+10)), ((x-10),  (y-10))]
            (x, y) = goalLoc game

initialState = Game
  { playerLoc = (0, 0), -- startPoint, handle input from algorithm
    goalLoc = (100, 100), -- endPoint, handle input from algorithm
    walls = [(1,1,'L'),(1,1,'U'),(1,2,'U'),(1,3,'L'),(1,3,'R'),(1,3,'U'),(2,1,'L'),(2,2,'U'),(2,3,'R'),(3,1,'L'),(3,1,'D'),(3,2,'L'),(3,2,'D'),(3,3,'L'),(3,3,'R'),(3,3,'D')],
    size = 3
  }

handleInput (EventKey (Char 'w') _ _ _) game =
  game { playerLoc = (x, (y+20)), goalLoc = (goalLoc game), walls = (walls game), size = (size game)}
  where
    (x, y) = playerLoc game
handleInput (EventKey (Char 'a') _ _ _) game =
  game { playerLoc = ((x-20), y), goalLoc = (goalLoc game), walls = (walls game), size = (size game) }
  where
    (x, y) = playerLoc game
handleInput (EventKey (Char 's') _ _ _) game =
  game { playerLoc = (x, (y-20)), goalLoc = (goalLoc game), walls = (walls game), size = (size game) }
  where
    (x, y) = playerLoc game
handleInput (EventKey (Char 'd') _ _ _) game =
  game { playerLoc = ((x+20), y), goalLoc = (goalLoc game), walls = (walls game), size = (size game) }
  where
    (x, y) = playerLoc game
handleInput (EventKey (Char 'r') _ _ _) game =
  game { playerLoc = (0, 0), goalLoc = (goalLoc game), walls = (walls game), size = (size game) }
handleInput _ game = game

handleGoal game =
  if (x2-20) < x && x < (x2+20) && (y2-20) < y && y < (y2+20) then game { playerLoc = (0, 0), goalLoc = (goalLoc game), walls = (walls game), size = (size game) }
     else game
        where
           (x, y) = playerLoc game
           (x2, y2) = goalLoc game

updateState _ game = handleGoal game

--main :: IO ()
--main = display window background drawing
main = play window background 30 initialState2 renderMaze handleInput updateState


-- TODO: remove this
initialState2 = Game
  { playerLoc = (-460, -460), -- startPoint, handle input from algorithm
    goalLoc = (100, 100), -- endPoint, handle input from algorithm
    walls = [(1,1,'L'),(1,1,'U'),(1,2,'L'),(1,2,'U'),(1,3,'L'),(1,3,'U'),(1,4,'U'),(1,5,'L'),(1,5,'U'),(1,6,'U'),(1,7,'L'),(1,7,'U'),(1,8,'U'),(1,9,'L'),(1,9,'U'),(1,10,'U'),(1,11,'L'),(1,11,'U'),(1,12,'L'),(1,12,'U'),(1,13,'L'),(1,13,'U'),(1,14,'L'),(1,14,'U'),(1,15,'U'),(1,16,'L'),(1,16,'U'),(1,17,'U'),(1,18,'L'),(1,18,'U'),(1,19,'U'),(1,20,'R'),(1,20,'U'),(2,1,'L'),(2,3,'U'),(2,5,'L'),(2,6,'U'),(2,7,'L'),(2,8,'U'),(2,9,'L'),(2,9,'U'),(2,11,'L'),(2,15,'U'),(2,17,'U'),(2,18,'L'),(2,18,'U'),(2,20,'L'),(2,20,'R'),(3,1,'L'),(3,1,'U'),(3,2,'U'),(3,3,'L'),(3,3,'U'),(3,4,'L'),(3,5,'L'),(3,6,'U'),(3,7,'L'),(3,8,'L'),(3,8,'U'),(3,9,'L'),(3,9,'U'),(3,12,'L'),(3,12,'U'),(3,14,'L'),(3,14,'U'),(3,15,'U'),(3,16,'L'),(3,16,'U'),(3,17,'U'),(3,18,'U'),(3,19,'L'),(3,20,'L'),(3,20,'R'),(3,20,'U'),(4,1,'L'),(4,1,'U'),(4,6,'U'),(4,7,'L'),(4,8,'L'),(4,9,'L'),(4,9,'U'),(4,10,'U'),(4,11,'L'),(4,12,'U'),(4,13,'U'),(4,14,'L'),(4,15,'L'),(4,16,'L'),(4,17,'U'),(4,18,'L'),(4,18,'U'),(4,20,'R'),(5,1,'L'),(5,1,'U'),(5,2,'U'),(5,3,'U'),(5,5,'L'),(5,6,'U'),(5,7,'L'),(5,8,'L'),(5,10,'U'),(5,11,'L'),(5,12,'L'),(5,13,'U'),(5,14,'L'),(5,14,'U'),(5,17,'L'),(5,18,'L'),(5,18,'U'),(5,19,'L'),(5,20,'R'),(5,20,'U'),(6,1,'L'),(6,1,'U'),(6,2,'U'),(6,3,'U'),(6,4,'L'),(6,4,'U'),(6,5,'U'),(6,8,'L'),(6,8,'U'),(6,10,'U'),(6,11,'L'),(6,12,'U'),(6,13,'U'),(6,15,'L'),(6,15,'U'),(6,16,'L'),(6,16,'U'),(6,17,'U'),(6,18,'L'),(6,20,'R'),(6,20,'U'),(7,1,'L'),(7,1,'U'),(7,2,'L'),(7,2,'U'),(7,4,'L'),(7,4,'U'),(7,5,'U'),(7,6,'L'),(7,6,'U'),(7,8,'L'),(7,8,'U'),(7,9,'U'),(7,12,'U'),(7,13,'U'),(7,14,'U'),(7,15,'L'),(7,16,'L'),(7,16,'U'),(7,17,'L'),(7,18,'L'),(7,18,'U'),(7,20,'L'),(7,20,'R'),(8,1,'L'),(8,2,'U'),(8,4,'L'),(8,4,'U'),(8,6,'L'),(8,6,'U'),(8,8,'U'),(8,9,'U'),(8,10,'L'),(8,11,'U'),(8,12,'U'),(8,13,'U'),(8,14,'U'),(8,15,'L'),(8,19,'L'),(8,20,'L'),(8,20,'R'),(8,20,'U'),(9,1,'L'),(9,1,'U'),(9,2,'L'),(9,2,'U'),(9,3,'L'),(9,4,'U'),(9,5,'L'),(9,6,'L'),(9,6,'U'),(9,7,'L'),(9,7,'U'),(9,8,'L'),(9,8,'U'),(9,10,'L'),(9,11,'L'),(9,12,'L'),(9,12,'U'),(9,13,'L'),(9,13,'U'),(9,14,'U'),(9,15,'L'),(9,15,'U'),(9,17,'L'),(9,17,'U'),(9,18,'U'),(9,19,'U'),(9,20,'R'),(10,1,'L'),(10,3,'L'),(10,3,'U'),(10,4,'L'),(10,9,'L'),(10,9,'U'),(10,11,'U'),(10,13,'L'),(10,14,'L'),(10,14,'U'),(10,16,'L'),(10,17,'L'),(10,18,'L'),(10,19,'U'),(10,20,'R'),(10,20,'U'),(11,1,'L'),(11,2,'L'),(11,3,'L'),(11,4,'U'),(11,5,'U'),(11,6,'U'),(11,7,'U'),(11,8,'L'),(11,10,'U'),(11,11,'L'),(11,11,'U'),(11,12,'L'),(11,12,'U'),(11,15,'U'),(11,16,'U'),(11,18,'L'),(11,19,'L'),(11,19,'U'),(11,20,'R'),(11,20,'U'),(12,1,'L'),(12,1,'U'),(12,3,'L'),(12,4,'L'),(12,4,'U'),(12,5,'U'),(12,6,'L'),(12,6,'U'),(12,8,'L'),(12,8,'U'),(12,9,'L'),(12,10,'U'),(12,11,'L'),(12,12,'L'),(12,13,'U'),(12,14,'U'),(12,15,'L'),(12,15,'U'),(12,16,'U'),(12,17,'L'),(12,17,'U'),(12,18,'U'),(12,19,'L'),(12,20,'L'),(12,20,'R'),(13,1,'L'),(13,1,'U'),(13,2,'L'),(13,3,'U'),(13,4,'L'),(13,5,'L'),(13,6,'U'),(13,10,'U'),(13,11,'L'),(13,13,'U'),(13,14,'L'),(13,14,'U'),(13,15,'L'),(13,16,'U'),(13,18,'L'),(13,18,'U'),(13,19,'U'),(13,20,'R'),(14,1,'L'),(14,2,'U'),(14,4,'U'),(14,6,'L'),(14,7,'U'),(14,8,'L'),(14,8,'U'),(14,9,'L'),(14,10,'L'),(14,10,'U'),(14,11,'L'),(14,12,'L'),(14,13,'U'),(14,15,'L'),(14,16,'L'),(14,16,'U'),(14,17,'U'),(14,18,'L'),(14,19,'U'),(14,20,'R'),(14,20,'U'),(15,1,'L'),(15,2,'L'),(15,2,'U'),(15,3,'U'),(15,4,'U'),(15,5,'U'),(15,7,'L'),(15,9,'L'),(15,9,'U'),(15,12,'U'),(15,13,'L'),(15,14,'L'),(15,14,'U'),(15,16,'U'),(15,19,'L'),(15,19,'U'),(15,20,'R'),(15,20,'U'),(16,1,'L'),(16,1,'U'),(16,2,'L'),(16,2,'U'),(16,3,'U'),(16,4,'L'),(16,4,'U'),(16,5,'U'),(16,6,'U'),(16,8,'U'),(16,10,'L'),(16,11,'L'),(16,12,'U'),(16,13,'U'),(16,14,'L'),(16,14,'U'),(16,15,'L'),(16,16,'U'),(16,17,'U'),(16,18,'U'),(16,20,'R'),(16,20,'U'),(17,1,'L'),(17,2,'U'),(17,5,'L'),(17,5,'U'),(17,7,'L'),(17,8,'L'),(17,9,'L'),(17,9,'U'),(17,11,'L'),(17,12,'L'),(17,13,'L'),(17,13,'U'),(17,15,'L'),(17,16,'U'),(17,17,'U'),(17,18,'L'),(17,18,'U'),(17,19,'U'),(17,20,'R'),(17,20,'U'),(18,1,'L'),(18,1,'U'),(18,2,'U'),(18,3,'U'),(18,5,'L'),(18,6,'L'),(18,6,'U'),(18,8,'L'),(18,9,'L'),(18,10,'L'),(18,11,'L'),(18,12,'L'),(18,14,'U'),(18,16,'U'),(18,17,'U'),(18,18,'U'),(18,20,'R'),(18,20,'U'),(19,1,'L'),(19,1,'U'),(19,2,'L'),(19,3,'U'),(19,4,'L'),(19,4,'U'),(19,6,'U'),(19,7,'U'),(19,8,'U'),(19,9,'U'),(19,10,'U'),(19,11,'L'),(19,12,'L'),(19,13,'L'),(19,14,'L'),(19,15,'U'),(19,16,'L'),(19,17,'L'),(19,18,'L'),(19,19,'L'),(19,20,'L'),(19,20,'R'),(20,1,'L'),(20,1,'D'),(20,2,'D'),(20,3,'U'),(20,3,'D'),(20,4,'L'),(20,4,'D'),(20,5,'L'),(20,5,'D'),(20,6,'U'),(20,6,'D'),(20,7,'L'),(20,7,'D'),(20,8,'U'),(20,8,'D'),(20,9,'U'),(20,9,'D'),(20,10,'U'),(20,10,'D'),(20,11,'U'),(20,11,'D'),(20,12,'L'),(20,12,'D'),(20,13,'L'),(20,13,'D'),(20,14,'L'),(20,14,'D'),(20,15,'L'),(20,15,'D'),(20,16,'L'),(20,16,'D'),(20,17,'L'),(20,17,'D'),(20,18,'U'),(20,18,'D'),(20,19,'L'),(20,19,'U'),(20,19,'D'),(20,20,'R'),(20,20,'D')],
    size = 20
  }
  