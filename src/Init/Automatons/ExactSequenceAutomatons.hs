module Init.Automatons.ExactSequenceAutomatons (
    exactSequenceAutomatons
) where

import Core.Automaton.Generator (generateAutoFromKeyword)
import Init.Common (Symbol(..), CommonAuto)

exactSymbols :: [(Symbol, String)]
exactSymbols = [
    (Public, "public"), (Static, "static"), (Class, "class"),
    (VoidType, "void"), (StringType, "String"),
    (MemberAccess, "."), (LPar, "("), (RPar, ")"), (LCBrac, "{"), (RCBrac, "}"), 
    (LSqBrac, "["), (RSqBrac, "]"), (StmtEnd, ";")
    ]

exactSequenceAutomatons :: [CommonAuto]
exactSequenceAutomatons = map (\s -> generateAutoFromKeyword (snd s) (fst s)) exactSymbols
