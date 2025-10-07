module Init.Tokenizer.Automatons.StringAutomaton (
    stringAutomaton
) where

import Core.Automaton.Commons (
    Automaton (Auto),
    AutomatonConstants (AutoConsts),
    AutomatonVars (AutoVars),
    initialVarsFromConstants
    )
import Init.Tokenizer.Common (CommonAuto, Symbol(..))

sInit = 0
sInside = 1
sEscaping = 2
sAccept = 3
sError = 4
stringAutomaton :: CommonAuto
stringAutomaton = Auto consts vars
    where
        whenInside c
            | c == '"' = sAccept
            | c == '\\' = sEscaping
            | otherwise = sInside
        transitionFunc state char
            | state == sInit = if char == '"' then sInside else sError
            | state == sInside = whenInside char
            | state == sEscaping = sInside
            | otherwise = sError
        consts = AutoConsts String sInit sError (sAccept ==) transitionFunc
        vars = initialVarsFromConstants consts
