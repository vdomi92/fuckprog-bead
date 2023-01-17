{-# LANGUAGE InstanceSigs #-}

import Data.Char
import Data.List

isDecimalDigit :: Char -> Bool
isDecimalDigit = isDigit

isInt :: String -> Bool
isInt (x:xs) = isDigit x && isInt xs 
