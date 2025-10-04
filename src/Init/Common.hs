module Init.Common (
    Symbol(..),
    CommonAuto
) where

import Data.Int (Int8)
import Core.Automaton.Commons (Automaton)

data Symbol =
    -- Can be matched as exact sequences
    Public | Static | Class
    | VoidType | StringType
    | MemberAccess | LPar | RPar | LCBrac | RCBrac | LSqBrac | RSqBrac | StmtEnd
    -- Require custom automatons
    | String | Identifier
    deriving (Read, Show, Enum, Eq)

-- We assume that a keyword is at most 127 characters long!
type CommonAuto = Automaton Int8 Symbol
