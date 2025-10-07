module Init.Tokenizer.Automatons.IdentifierAutomaton (
    identifierAutomaton
) where

import Data.Char (isAsciiUpper, isAsciiLower, isDigit)
import Core.Automaton.Commons (
    Automaton (Auto),
    AutomatonConstants (AutoConsts),
    AutomatonVars (AutoVars),
    initialVarsFromConstants
    )
import Init.Tokenizer.Common (CommonAuto, Symbol(..))

isValidStart :: Char -> Bool
isValidStart c = isAsciiLower c || isAsciiUpper c || c == '_'

sInit = 0
sAccept = 1
sError = 2
identifierAutomaton :: CommonAuto
identifierAutomaton = Auto consts vars
    where
        transitionFunc state char
            | state == sInit = if isValidStart char then sAccept else sError
            | state == sAccept = if isValidStart char || isDigit char then sAccept else sError
            | otherwise = sError
        consts = AutoConsts Identifier sInit sError (sAccept ==) transitionFunc
        vars = initialVarsFromConstants consts
