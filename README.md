# Welcome to Mazekell!
---
<img src="https://github.com/CPSC-312-Haskell-Project/mazekell/blob/master/Maze.png" alt="Maze" width="300" height="300">
Project URL: https://wiki.ubc.ca/Course:CPSC312-2019-Maze-Generator

## Generating a maze:
### Option 1: Compile and run using GHC (required on macOS):

```console
ghc -- -main-is MazeCli MazeCli.hs -o main -threaded && ./main
```
If you want to use a seed:
```console
ghc -- -main-is MazeCli MazeCli.hs -o main -threaded && ./main 1234
```
### Option 2: Using GHCI:

```console 
ghci MazeCli.hs
:set args "12341"    // This is the settable seed (Integer)
main
```

## How to play:
1. Use WASD or Arrow Keys to make your way to the end of the maze.
2. Press R to reset your position to the start of the maze.
3. Press Escape to close the game.

## Analysis:

### Maze Generation:

We used randomized Prim’s algorithm to generate mazes. We represented maze walls as triplets (r, c, w), where r is the row number and c is the column number of the cell in the grid, and w is one of ‘L’, ‘R’, ‘U’, ‘D’, representing if it is the left, right, ceiling or floor wall of the cell.

Implementing the algorithm was not space efficient because Haskell does not allow us to save state in pure functions. So each recursive call created a new data structure for the next call instead of just utilizing the already evaluated structure. This made the space complexity of the algorithm to be exponential in the number of walls in the maze, because Haskell creates new arrays and sets on every iteration of the algorithm. The following data was collected:

| Size of Maze	| Memory used | 
| ---- | ---- |
| 10 |	4 MB |
| 20	| 26 MB |
| 50	| 400 MB |
| 100 |	3 GB |

A language that allows saving state, such as Java, Python, etc. would be more space efficient to use over Haskell.

On the other hand, the time complexity of the algorithm is fairly reasonable. If there are n walls in a maze, the time complexity to generate that maze is O(n^2)

### GUI:

In terms of the GUI, Gloss made the setup and development process smoother. Because the algorithm produced a list of walls to be drawn, we were able to implement a recursive structure to place the walls at the right coordinates on the screen. Functional programming languages work great for these recursively build games. However, with each "update" of the game, i.e. every move, the entire game would be redrawn, which is not efficient.

Overall, Haskell has potential for small 2D games and the algorithms that drive these games. The benefits of using Haskell for this project was that Haskell was easily debuggable because it is strongly typed, and reasoning about and making changes to the current game state was very readable and simple to introduce. The major drawbacks, space and time complexity, are largely due to the nature of functional programming languages.

In terms of future improvements, building upon our GUI aesthetically and supporting additional game elements such as an in game menu (using gloss), music (http://hackage.haskell.org/package/haskell-player), etc. would be interesting and reasonable paths to pursue using Haskell. There is also potential for implementing alternative Maze generator algorithm.
