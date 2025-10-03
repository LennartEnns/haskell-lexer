{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Core.Automaton.Generator (
    generateAutoFromKeyword
) where

import Core.Automaton.Commons (
    Automaton (Auto),
    AutomatonConstants (AutoConsts),
    AutomatonVars (AutoVars),
    initialVarsFromConstants
    )

-- Generates an automaton that matches an exact keyword
generateAutoFromKeyword :: String -> symbolType -> Automaton Int symbolType
generateAutoFromKeyword keyword symbol = Auto consts vars
    where
        kwLen = (length keyword)
        errState = kwLen + 1
        transitionFunc state char = if ((state < kwLen) && ((keyword !! state) == char)) then (state + 1) else errState
        -- kwLen is the index of our accepting state
        consts = AutoConsts symbol 0 errState (kwLen ==) transitionFunc
        vars = initialVarsFromConstants consts
