-- This module defines the symbols and automatons for our specific usecase
module Init.InitAutomatons (
    initAutomatons
) where

import Init.Common (CommonAuto)
import Init.Automatons.CommentAutomaton (commentAutomaton)
import Init.Automatons.ExactSequenceAutomatons (exactSequenceAutomatons)
import Init.Automatons.StringAutomaton (stringAutomaton)
import Init.Automatons.IdentifierAutomaton (identifierAutomaton)

initAutomatons :: [CommonAuto]
initAutomatons = [commentAutomaton] ++ exactSequenceAutomatons ++ [stringAutomaton] ++ [identifierAutomaton]
