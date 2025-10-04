module Core.Utils (
    trimStart,
    intToInt8,
    int8ToInt
) where

import Data.Char (isSpace)
import Data.Int (Int8)

trimStart :: String -> String
trimStart [] = ""
trimStart (c:cs) = if isSpace c then trimStart cs else c:cs

intToInt8 :: Int -> Int8
intToInt8 = fromIntegral

int8ToInt :: Int8 -> Int
int8ToInt = fromIntegral
