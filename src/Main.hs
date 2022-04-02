module Main where

import System.Console.Haskeline
import Data.List (isPrefixOf)
import Control.Monad.State.Strict (get, runStateT, StateT)

import Parsing
import Expr
import REPL

-- Reference: https://www.reddit.com/r/haskell/comments/1os0yq/haskeline_woes/
findCompletion :: String -> StateM [Completion]
findCompletion s = do map simpleCompletion . filter (s `isPrefixOf`) . wrds <$> get

hlSettings :: Settings (StateT LState IO)
hlSettings = setComplete (completeWord Nothing " \t" findCompletion) defaultSettings

main :: IO ((), LState)
main = runStateT (runInputT hlSettings repl) initLState