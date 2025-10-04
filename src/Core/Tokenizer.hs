module Core.Tokenizer (
    tokenize
) where

import Core.Automaton.Commons (
    Automaton(..),
    AutomatonVars(..),
    AutomatonConstants(..),
    stepAuto,
    toInit,
    isTrapped
    )
import Core.Utils (trimStart)
import Data.Maybe (maybeToList, isJust, isNothing)
import Data.Char (isSpace)
import Distribution.Utils.String (trim)

data TokenizerState autoStateType symbolType = TzerState {
    autos :: [Automaton autoStateType symbolType], -- The modified automatons
    firstAcceptingSymbol :: Maybe symbolType, -- Symbol of the first (most precedent) automaton that is accepting, or Nothing if none is accepting
    allTrapped :: Bool -- Whether all automatons are trapped (in error state)
}

initialTokenizerStateFromAutos :: [Automaton stt symt] -> TokenizerState stt symt
initialTokenizerStateFromAutos autos = TzerState autos Nothing False

-- Reset tokenizer state, including all automatons
resetTokenizerState :: TokenizerState stt symt -> TokenizerState stt symt
resetTokenizerState (TzerState autos _ _) = TzerState (map toInit autos) Nothing False

-- Helper function for the fold operation applied in stepAllAutos
allAutosFoldFunc :: (Eq stateType) => Char -> Automaton stateType symbolType ->
    TokenizerState stateType symbolType -> TokenizerState stateType symbolType
allAutosFoldFunc c autoBeforeStep (TzerState newAutos firstAccSymbol allTrapped) =
    TzerState (autoAfterStep : newAutos) newSymbol newAllTrapped
    where
        autoAfterStep = stepAuto c autoBeforeStep
        newSymbol = if isAccepting (vars autoAfterStep)
            then Just (associatedSymbol (constants autoAfterStep))
            else firstAccSymbol
        newAllTrapped = allTrapped && isTrapped autoAfterStep

-- Step all automatons by reading the current character
-- Returns the new tokenizer state
stepAllAutos :: (Eq stateType) => Char -> [Automaton stateType symbolType] -> TokenizerState stateType symbolType
stepAllAutos c = foldr foldFunc (TzerState [] Nothing True)
    where foldFunc = allAutosFoldFunc c

-- Tokenizer function. Converts a string to symbols using the provided automatons.
-- Receives:
-- - Current state of the tokenizer process
-- - The remaining characters to tokenize
-- Returns:
-- - The resulting symbols, if any
-- - Whether the tokenization was successful (valid text)
tokenizeRec :: (Eq stateType) => TokenizerState stateType symbolType -> String -> ([symbolType], Bool)

tokenizeRec (TzerState _ firstAccSymbol _) [] = (symbols, success)
    where
        symbols = maybeToList firstAccSymbol -- Empty if no symbol, else [symbol]
        success = isJust firstAccSymbol -- Success if symbol exists

tokenizeRec oldState (c:cs) = if error then (newSymbols, False) else (newSymbols ++ symbolsFromRest, successFromRest)
    where
        (TzerState oldAutos oldFas oldAllTrapped) = oldState -- Destructure for easier property access
        whitespace = isSpace c

        updatedState = stepAllAutos c oldAutos
        (TzerState _ _ newAllTrapped) = updatedState

        newSymbols = if newAllTrapped then maybeToList oldFas else []

        -- Stop scanning if new step is trapped and there was no accepted symbol on the previous character
        error = newAllTrapped && isNothing oldFas

        ----------------- These are lazily evaluated only when error = False ---------------
        stateForRest
            -- If all automatons are trapped after reading the current character, reset state before next call
            | newAllTrapped = resetTokenizerState updatedState
            | otherwise = updatedState
        stringForRest
            | not newAllTrapped = cs
            | whitespace = trimStart cs
            | not whitespace = c:cs -- Repeat current character if we are trapped on it

        (symbolsFromRest, successFromRest) = tokenizeRec stateForRest stringForRest

tokenize :: (Eq stateType) =>  [Automaton stateType symbolType] -> String -> ([symbolType], Bool)
tokenize autos s = tokenizeRec (initialTokenizerStateFromAutos autos) (trim s)
