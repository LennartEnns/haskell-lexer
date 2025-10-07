module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Data.Maybe (maybe)
import Init.Simple.InitAutomaton (simpleTokenize)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [stmt] -> do
            let result = simpleTokenize stmt
            case result of
                Just val -> do
                    putStrLn ("Successfully executed the automaton and got the symbol '" ++ show val ++ "'")
                    exitSuccess
                _ -> do
                    putStrLn "Error: Automaton did not end in an accepting state"
                    exitFailure
        _ -> do
            putStrLn "Usage:"
            putStrLn "  myprog <text>"
            exitFailure
