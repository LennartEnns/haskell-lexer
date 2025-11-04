{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Core.Automaton.Commons (
    Automaton(..),
    AutomatonConstants(..),
    AutomatonVars(..),
    stepAuto,
    isTrapped,
    initialVarsFromConstants,
    toInit
) where

-- Constant (never-changing) properties of the automaton
data AutomatonConstants stateType symbolType = AutoConsts {
    associatedSymbol :: symbolType, -- Symbol that this automaton matches
    initState :: stateType,
    errorState :: stateType, -- The trap state that only transitions to itself
    isAcceptingFunc :: stateType -> Bool, -- Whether the current state is an accepting state
    transitionFunc :: stateType -> Char -> stateType -- Maps (current state, new symbol) to a new statex
}

-- Variable properties of the automaton. Updated after each transition.
data AutomatonVars stateType = AutoVars {
    state :: stateType, -- Current state
    isAccepting :: Bool -- Whether the current state is accepting
}

data Automaton stateType symbolType = Auto {
    constants :: AutomatonConstants stateType symbolType,
    vars :: AutomatonVars stateType
}

-- An updated version of the automaton based on the passed character
stepAuto :: Char -> Automaton stateType symbolType -> Automaton stateType symbolType
stepAuto c auto = auto { vars = AutoVars newState newAccepting }
  where
    oldState     = state (vars auto)
    newState     = (transitionFunc (constants auto)) oldState c
    newAccepting = (isAcceptingFunc (constants auto)) newState

-- Whether the automaton is in its error state (trap)
isTrapped :: (Eq stateType) => Automaton stateType symbolType -> Bool
isTrapped auto = (state (vars auto)) == (errorState (constants auto))

-- Initialized automaton vars based on the given constants
initialVarsFromConstants :: AutomatonConstants stateType symbolType -> AutomatonVars stateType
initialVarsFromConstants consts = AutoVars state (isAcceptingFunc consts state)
    where state = initState consts

-- Set the automaton to its initial state
toInit :: Automaton stateType symbolType -> Automaton stateType symbolType
toInit auto = auto { vars = initialVarsFromConstants (constants auto) }
