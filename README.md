# lifeHS
Simple implementation of [Conway's Game of Life][1] in Haskell using a finite wraparound grid.

## Getting Started

Simply run the binary file: `./life`

The starting state of the game is taken from the contents of `initialState.txt`; you can edit this file to obtain different results. Use `#` to denote alive cells and `.` to denote dead cells. (Also, make sure the grid is a perfect rectangle!) For example , here is a 10x10 grid containing a single 'glider':

    ..........
    ..........
    ...#......
    ....#.....
    ..###.....
    ..........
    ..........
    ..........
    ..........
    ..........
    
If you need to recompile the source code, use GHC: `ghc --make life`

[1]: https://en.wikipedia.org/wiki/Conway's_Game_of_Life
