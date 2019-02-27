# tic-tac-toe

[![Build Status](https://travis-ci.com/ajeetdsouza/tic-tac-toe.svg?branch=master)](https://travis-ci.com/ajeetdsouza/tic-tac-toe)

A simple tic-tac-toe game written in Haskell. Supports 3 agents: `Human`, `Random` and `Minimax`. Moves entered must be between `A1` and `C3`. 

## Building
Once you have Stack set up, simply `cd` into the root directory and:
```sh
stack build
stack exec -- tic-tac-toe
```

## Extending
More agents can easily be added to the `Player` data type. The `getMove :: Board -> Token -> IO Move` function must be implemented.

