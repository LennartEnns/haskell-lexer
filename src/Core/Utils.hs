module Core.Utils (
    trimStart
) where

import Data.Char (isSpace)

trimStart :: String -> String
trimStart [] = ""
trimStart (c:cs) = if isSpace c then trimStart cs else c:cs
