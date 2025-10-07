-- This module defines the symbols and automatons for the tokenizer PoC
module Init.Simple.InitAutomaton (
    simpleTokenize
) where

import Data.List (findIndex)
import Data.Maybe (maybe)
import Core.Automaton.Commons (
    Automaton(..),
    AutomatonConstants (AutoConsts),
    AutomatonVars(..),
    initialVarsFromConstants,
    stepAuto,
    isTrapped
    )

-- Currently manual creation of states/transitions
-- Transition graph might be generated in the future
type State = (Int, Int)
initState = (0, (-1))
errState = ((-1), (-1))

-- In our API, each automaton must have an associated symbol (used by the tokenizer, but not needed here)
data DummySymbol = DummySymbol
    deriving (Read, Show, Enum, Eq)

-- Actual symbols we want to output
data Symbol = Public | Static | Class | Void
    deriving (Read, Show, Enum, Eq)

-- We assume that a keyword is at most 127 characters long!
type SimpleAuto = Automaton State DummySymbol

keywords = ["public", "static", "class", "void"]
symbols = [Public, Static, Class, Void]

-- Transition function for non-special states
transFromKeywords (kwIndex, cIndex) char
    | cIndex < (length kw) = if correctChar then (kwIndex, cIndex + 1) else errState
    | otherwise = errState
        where
            kw = keywords !! kwIndex
            correctChar = char == (kw !! cIndex)

isStateAccepting :: State -> Bool
isStateAccepting (kwIndex, cIndex) = (kwIndex >= 0) && (kwIndex < (length keywords)) && (cIndex == (length (keywords !! kwIndex)))

initAutomaton :: SimpleAuto
initAutomaton = Auto consts vars
    where
        firstKwWithChar expected = findIndex (\(c:cs) -> c == expected) keywords

        trf state c
            | state == initState = maybe errState (\i -> (i, 1)) (firstKwWithChar c)
            | state == errState = errState
            | otherwise = transFromKeywords state c
        consts = AutoConsts DummySymbol initState errState isStateAccepting trf
        vars = initialVarsFromConstants consts

simpleTokenizeRec :: SimpleAuto -> String -> Maybe Symbol
simpleTokenizeRec auto []
    | isAccepting (vars auto) = Just (symbols !! (fst (state (vars auto))))
    | otherwise = Nothing
simpleTokenizeRec auto (c:cs)
    | isTrapped nextAuto = Nothing
    | otherwise = simpleTokenizeRec nextAuto cs
        where
            nextAuto = stepAuto c auto

simpleTokenize :: String -> Maybe Symbol
simpleTokenize (char:chars) = simpleTokenizeRec initAutomaton (char:chars)
