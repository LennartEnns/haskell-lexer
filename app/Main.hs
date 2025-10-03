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
        [stmt] -> runTokenizer stmt            -- passed target text directly
        ["-f", fileName] -> do                 -- passed path to file that contains target text
            content <- readFile fileName
            runTokenizer content
        _ -> do
            putStrLn "Usage:"
            putStrLn "  myprog <text>"
            putStrLn "  myprog -f <file>"

runTokenizer :: String -> IO ()
runTokenizer stmt = do
    let (symbols, success) = tokenize autos stmt
    putStrLn ("Tokenizer was " ++ (if success then "" else "not ") ++ "successful")
    putStrLn ("Successful symbols:\n" ++ show symbols)
    if success then exitSuccess else exitFailure
