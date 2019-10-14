To generate maze with GUI:

```
ghc -- -main-is MazeCli MazeCli.hs -o main -threaded && ./main
```

To generate a maze, run the following commands:

```console 
ghci MazeCli.hs
:set args "12341"    // This is the seed
main
```

To run GUI, use this command:

```
ghc -- GUI.hs -o main -threaded && ./main
```