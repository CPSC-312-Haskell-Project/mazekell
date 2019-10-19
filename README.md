Welcome to Mazekell!
---

To generate maze with GUI:

```
ghc -- -main-is MazeCli MazeCli.hs -o main -threaded && ./main
```

To generate a maze, run the following commands:

```console 
ghci MazeCli.hs
:set args "12341"    // This is the settable seed (Integer)
main
```

To run GUI, use this command:

```
ghc -- GUI.hs -o main -threaded && ./main
```

Controls:
```
Use WASD or Arrow Keys to make your way to the end of the maze.
Press R to reset your position to the start of the maze.
Press Escape to close the game.
```