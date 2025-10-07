-- This module defines the symbols and automatons for the tokenizer PoC
module Init.Tokenizer.InitAutomatons (
    initAutomatons
) where

import Init.Tokenizer.Common (CommonAuto)
import Init.Tokenizer.Automatons.CommentAutomaton (commentAutomaton)
import Init.Tokenizer.Automatons.ExactSequenceAutomatons (exactSequenceAutomatons)
import Init.Tokenizer.Automatons.StringAutomaton (stringAutomaton)
import Init.Tokenizer.Automatons.IdentifierAutomaton (identifierAutomaton)

initAutomatons :: [CommonAuto]
initAutomatons = [commentAutomaton] ++ exactSequenceAutomatons ++ [stringAutomaton] ++ [identifierAutomaton]
