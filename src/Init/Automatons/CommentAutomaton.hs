module Init.Automatons.CommentAutomaton (
    commentAutomaton
) where

import Core.Automaton.Commons (
    Automaton (Auto),
    AutomatonConstants (AutoConsts),
    AutomatonVars (AutoVars),
    initialVarsFromConstants
    )
import Init.Common (CommonAuto, Symbol(..))

sInit = 0
sOneSlash = 1
sAccept = 2
sError = 3
commentAutomaton :: CommonAuto
commentAutomaton = Auto consts vars
    where
        transitionFunc state char
            | state == sInit = if char == '/' then sOneSlash else sError
            | state == sOneSlash = if char == '/' then sAccept else sError
            | state == sAccept = if char /= '\n' then sAccept else sError
            | otherwise = sError
        consts = AutoConsts Comment sInit sError (sAccept ==) transitionFunc
        vars = initialVarsFromConstants consts
