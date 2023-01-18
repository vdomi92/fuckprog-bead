{-# LANGUAGE InstanceSigs #-}

import Data.Char
import Data.List
import Text.Read

isDecimalDigit :: Char -> Bool
isDecimalDigit = isDigit

isInt :: String -> Bool
isInt str 
    | str == "" = False
    | str == (x:"") = isDigit x
    | str == (x:xs) = isDigit x && isInt xs


--isInt "" = False
--isInt (x:"") = isDigit x
--isInt (x:xs)  = isDigit x && isInt xs







-- case dropWhile isDigit xs of
--                        ""       -> True
--                        ('.':ys) -> all isDigit ys
--                        _        -> False
