{-# LANGUAGE InstanceSigs #-}

import Data.Char
import Data.List
import Text.Read hiding (get)

--Döntsd el egy karakterről, hogy decimális (tízes számrendszerbeli) számjegyet ábrázol-e vagy sem!
isDecimalDigit :: Char -> Bool
isDecimalDigit = isDigit

--Vizsgáld meg egy szóközöket nem tartalmazó karaktersorozatról, hogy természetes számot ábrázol-e vagy sem (csak tízes számrendszerbeli számjegyekből áll)! Előjelekkel az egyszerűség kedvéért most nem kell foglalkozni.
isInt :: String -> Bool
isInt = all isDigit

--Készíts egy olyan függvényt, amely megadja egy String-ként leírt szám értékét egész számként (Int)! Feltételezhető, hogy a bemenet jólformált, vagyis az isInt függvény igazat ad rá.
strToInt :: String -> Int
strToInt str = read str :: Int

--Következő lépésben készítünk egy verem adatszerkezet:
type Stack a = [a]
--A type kulcsszó típusszinonimát vezet be, tehát a Stack a igazából mindig csak egy listát jelent, amelynek az elemtípusa a. Készítsd el a következő két veremműveletet!
--A push művelet a verem tetejére tesz egy elemet.
push :: Stack a -> a -> Stack a
push stack x = x : stack

--A pop művelet kiveszi a verem legfelső elemét, és egy párban visszaadja ezt az elemet, illetve az elem kivételével megkapott vermet (amelynek eggyel kisebb így a mérete, mint az eredeti veremnek). Feltételezhető, hogy a veremnek van legalább egy eleme.
pop :: Stack a -> (a, Stack a)
--pop [] = error "Error, empty stack", nvm feltételezhető, hgoy legalább 1 elemű.
pop (x:xs) = (x, xs)

--Adj meg egy függvényt, amely egy szöveget választ ketté az első szóköz mentén! Az eredmény egy pár, amelynek az első tagja az input első szava (első szóközig tartó karaktersorozat). Az eredmény második tagja az input második szavától kezdődő szuffixuma. Figyelj rá, hogy nem biztos, hogy egyetlen szóköz választja el a szavakat (azaz az első és második szó között lévő összes szóközt hagyd el a szuffixum elejéről)!
breakOnSpace :: String -> (String, String)
breakOnSpace input = (fst splitted, dropWhile (==' ') (snd splitted))
  where filtered = break (==' ') (dropWhile (==' ') input)
        splitted = (fst filtered, dropWhile (==' ') (snd filtered))

--Add meg a get függvényt, amely egy kulcs-érték párok listájából kiválasztja az adott kulcshoz tartozó értéket! A kulcsok típusán értelmezett az egyenlőség, de nem feltétlenül rendezhetőek. Feltételezhető, hogy egy kulcs maximum egyszer szerepel a listában. Ha egy kulcs nem szerepel a listában, legyen Nothing az eredmény!
get :: Eq a => [(a, b)] -> a -> Maybe b
get [] _ = Nothing
get (x:xs) key = if (fst x) == key then Just (snd x) else get xs key