# H2
This is the second practical for CS2006. This folder has a .cabal file for building
and testing the scripting language program as well as a couple of text files for testing the import functionality.
The src folder contain all the .hs files for the scripting language and for testing.

## Requirements
QuickCheck must be installed to run the tests for the scripting language. We recommend using cabal to install it using the terminal
with the following command.

    $ cabal install QuickCheck
    
## Scripting Language
To run the scripting language, cd to the Haskell folder and run the following commands
    
    $ cabal build
    $ cabal run H2

## Test
To run the test, cd to the Haskell folder and run the following commands
    
    $ cabal build
    $ cabal run test
