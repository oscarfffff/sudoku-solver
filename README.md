# SMT Sudoku
This Project implements a Sudoku instance generator based on SMT. It uses the Haskell library *hasmtlib* to model the problem as an SMT instance and solves it via the Z3 solver.

## Prerequisite
- Haskell with cabal (can be installed via *ghcup*)
- Z3 (although others are possible - can be changed via the Solver.hs file)

## Build and run
To build the project run
```` bash
cabal build
````
For a clean build (for example after changing the solver) run
```` bash
cabal clean && cabal build
````
After that you can run it via
```` bash
cabal run
````
By default it starts to generate a new Sudoku instance, so no inputs are neccessary. The solver can be used as a standalone component.

## More Information
More detailed information on the project and implementation can be found in *smt_sudoku_fmw.pdf*
