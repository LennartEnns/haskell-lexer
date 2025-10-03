module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import InitAutomatons (initAutomatons)
import Core.Tokenizer (tokenize)

autos = initAutomatons

main :: IO ()
main = do
    args <- getArgs
    case args of
        [stmt] -> do
            let (symbols, success) = tokenize autos stmt
            putStrLn ("Tokenizer was " ++ (if success then "" else "not ") ++ "successful")
            putStrLn ("Successful symbols:\n" ++ show symbols)
            if success then exitSuccess else exitFailure
        _ -> do
            putStrLn "You must supply exactly one argument"
            exitFailure
