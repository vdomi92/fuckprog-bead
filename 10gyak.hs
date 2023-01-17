{-# LANGUAGE InstanceSigs #-}

import Prelude hiding (Maybe, Either, Left, Right, Nothing, Just)  -- mai oran vett anyagokat elrejtjuk a Prelude modulbol

-- Kiegeszites: foldr, foldl, foldl', foldr1, foldl1 hasznalata

----------------------------------------------
-- Algebrai adattipusok, deriving, Eq, Show --
----------------------------------------------

-- 1. Felsorolasi tipusok

-- Weekday

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show)

-- mintaillesztes
isMonday :: Weekday -> Bool
isMonday Mon = True
isMonday _   = False

-- isMonday x = x == Mon  -- Eq tipusosztaly kell

isWeekend :: Weekday -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _   = False

-- 2 elemu tipus? -- Bool
-- 1 elemu tipus? -- (), Unit, Top
-- 0 elemu tipus? -- Bottom, undefined

-- 2. Parameteres adatkonstruktorok:
-- Time ~ szorzat tipus

data Time = T Int Int -- deriving Show -- parameteres konstruktor


instance Show Time where  -- tipusosztaly peldanya
  show :: Time -> String
  show (T x y) = show x ++ ":" ++ show y

instance Eq Time where
  (==) :: Time -> Time -> Bool
  -- T x1 y1 == T x2 y2 = undefined
  (==) (T x1 y1) (T x2 y2) = x1 == x2 && y1 == y2
-- izomorf a (Int, Int) tipussal, azaz meg tudunk adni olyan 
--   f :: Time -> (Int, Int) es g :: (Int, Int) -> Time fuggvenyeket, 
--   hogy minden x::(Int, Int)-re: (f . g) x == x es y::Time-ra (g . f) y == y

fTime :: Time -> (Int, Int)
fTime (T x y) = (y, x)

gTime :: (Int, Int) -> Time
gTime (x, y) = T y x

-- Izomorfizmus tesztek:
input1 :: [(Int, Int)]
input1 = zip [0..23] [0..59]
input2 :: [Time]
input2 = zipWith T [0..23] [0..59]

test1 = input1 == map (fTime . gTime) input1
test2 = input2 == map (gTime . fTime) input2

-- MJ: ha a Haskellnel egy meg "szigorubb" tipusozasu nyelvet hasznalnank (pl. Coq, Agda), akkor ezt
--     nem tesztelnenk, hanem bizonyitanank tetszoleges inputra

validTime :: Time -> Bool
validTime = undefined

-- 3. Tipusparameteres adattipusok
-- 3. a) Maybe

data Maybe a = Nothing | Just a deriving (Eq, Show)

safeDiv :: Int -> Int -> Maybe Int
safeDiv = undefined

-- Keszitsunk egy map-ot: kulcs-ertek parok listaja. A kulcsok legyenek Intek, ertekek tetszoleges tipusuak.
-- Legyen put, get muvelet (egyszeruseg kedveert csak (==)-t vizsgaljunk, HF: rendezetten megvalositott map)
type Map a = [(Int, a)]

put :: Int -> a -> Map a -> Map a
put k v []     = [(k, v)]
put k v (x:xs) = if fst x /= k then x:put k v xs else (k, v):xs

get :: Int -> Map a -> Maybe a
get = undefined

-- 3. b) Either
-- osszeg tipus:
data Either a b = Left a | Right b deriving (Eq, Show)

-- Adj meg egy Int-eket es String-eket tartalmazo mapot!
-- exampleMap :: ?
-- exampleMap = undefined

-- izomorfizmus ujra:
-- Mutasd meg, hogy az Either a () tipus izomorf a Maybe a tipussal
f :: Maybe a -> Either a ()
f = undefined

g :: Either a () -> Maybe a
g = undefined


data Shape2D = Circle Double               -- sugar
             | Rectangle Double Double     -- oldalak hossza
  deriving Show
type Shape3D = (Shape2D, Double) -- alap + magassag

draw :: Shape2D -> Double -> Either Shape2D Shape3D
draw = undefined

