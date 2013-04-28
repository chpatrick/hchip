# hchip
## Haskell Chip16 emulator

Specification and ROMs [here](https://github.com/tykel/chip16).

Controls
--------
A: Z  
B: X  
Start: Q  
Select: W 
D-pad: arrow keys

How to build
------------
You will need ghc and cabal-install. From the root directory, run:

    cabal install --only-dependencies
    cabal configure
    cabal build
