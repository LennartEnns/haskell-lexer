-- This module defines the symbols and automatons for our specific usecase
module InitAutomatons (
    initAutomatons
) where

import Core.Automaton.Generator (generateAutoFromKeyword)

import Core.Automaton.Commons (
    Automaton (Auto),
    AutomatonConstants (AutoConsts),
    AutomatonVars (AutoVars)
    )

import Data.Char (isAsciiUpper, isAsciiLower, isDigit)


data Symbol = Public | Static | Void | Identifier
    deriving (Read, Show, Enum, Eq)

exactMatchSymbols :: [(Symbol, String)]
exactMatchSymbols = [(Public, "public"), (Static, "static"), (Void, "void")]

exactMatchAutomatons :: [Automaton Int Symbol]
exactMatchAutomatons = map (\s -> generateAutoFromKeyword (snd s) (fst s)) exactMatchSymbols


isValidStart :: Char -> Bool
isValidStart c = isAsciiLower c || isAsciiUpper c || c == '_'

identInit = 0
identAccept = 1
identError = -1
identifierMatchAutomaton :: Automaton Int Symbol
identifierMatchAutomaton = Auto consts vars
    where
        transitionFunc state char
            | state == identInit = if isValidStart char then identAccept else identError
            | state == identAccept = if isValidStart char || isDigit char then identAccept else identError
            | otherwise = identError
        consts = AutoConsts Identifier identInit identError (identAccept ==) transitionFunc
        vars = AutoVars identInit False

initAutomatons :: [Automaton Int Symbol]
initAutomatons = exactMatchAutomatons ++ [identifierMatchAutomaton]
