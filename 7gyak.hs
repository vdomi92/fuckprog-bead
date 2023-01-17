import Data.Char

-- Add meg az osszefesulo rendezes osszefesulo lepeset (merge fuggveny)!
-- Ebben a lepesben ket listat kell rendezetten osszefesulni.
merge :: Ord a => [a] -> [a] -> [a]
merge [] []         = []
merge [] ys         = ys
merge xs []         = xs
merge (x:xs) (y:ys)
  | x < y     = x : merge xs     (y:ys)
  | otherwise = y : merge (x:xs) ys

--merge (x:xs) l@(y:ys)   --- alias minta
--  | x < y     = x : merge xs     l
--  | otherwise = y : merge (x:xs) ys

-- rekurziv HAZI FELADAT:
-- Adott egy adatbazis (map adatszerkezet), amely kulcsokhoz rendel ertekeket. Az adatbazist
-- rendezett modon abrazoljuk. Add meg adott kulcshoz tartozo erteket!
-- Adj hibat, ha az adott kulcs nincs benne az adatbazisban
getValue :: Ord a => [(a, b)] -> a -> b
getValue = undefined

-- rekurziv HAZI FELADAT:
-- Szurj be egy kulcs-ertek part az adatbazisba (meglevo kulcs felulirasaval)
insert :: Ord a => [(a, b)] -> a -> b -> [(a, b)]
insert = undefined




-- szorozd meg egy lista paros elemeit kettovel!
double :: [Int] -> [Int]
double [] = []
double (x:xs)
  | even x    = 2 * x : double xs
  | otherwise =     x : double xs

-- Transzormald ASCII kodok listajat szovegge
toString :: [Int] -> String
toString []     = []
toString (x:xs) = chr x : toString xs

-- teszt: [97,108,109,97,102,97]
-- Mi a kozos?
mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x:xs) = f x : mymap f xs

toString' :: [Int] -> String
toString' xs = mymap chr xs

toString'' :: [Int] -> String
toString'' = mymap chr

myeven :: Int -> Int
myeven x = if even x then 2 * x else x

double' xs = mymap myeven xs

double'' xs = mymap (\x -> if even x then 2 * x else x) xs

-- Hany darab paratlan eleme van egy listanak?
oddcount :: [Int] -> Int
oddcount xs = length (odds xs)
  where
    odds []     = []
    odds (x:xs) = if odd x
                  then x : odds xs
                  else     odds xs

-- Szurd ki egy lista azon elemeit, amelyeknek a 13-mal vett osztasi maradeka paros!
mod132 :: [Int] -> [Int]
mod132 []     = []
mod132 (x:xs) = if even (x `mod` 13)
                then x : mod132 xs
                else     mod132 xs

-- mi a kozos?
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ []     = []
myfilter p (x:xs) = if p x
                    then x : myfilter p xs
                    else     myfilter p xs

odds' xs = myfilter odd xs

-----------------------------
-- intervallum kifejezesek --
-----------------------------

-- [i, j .. k]
  --  i = elso elem
  --  j = potencialis masodik elem (d-tol fugg)
  --  k = potencialis utolso elem (d-tol fugg)
  --  differencia (d) = j - i
-- sorold fel az szamokat m-tol n-ig!
nums1 m n = [m..n]

-- sorold fel a paros szamokat m-tol n-ig!
nums2 m n = if even m
            then [m, m+2..n]
            else [m+1, m+3..n]

-- intervallum kifejezesek tortekre: felso hatar differencia/2 "sugaru kornyezeteben" van az utolso ertek 
floats1 = [1.1, 1.3 .. 2.2]
floats2 = [1.1, 1.3 .. 2.19]

-----------------------
-- halmazkifejezesek --
-----------------------

-- Add meg egy lista paros elemeinek duplajat!
doubleEvens :: [Int] -> [Int]
doubleEvens xs = [x * 2 | x <- xs, even x]

-- mappal es filterrel?
doubleEvens' :: [Int] -> [Int]
doubleEvens' xs = map (2*) (filter even xs) 

-- Mit kene atirni, hogyha nem szeretnenk a paratlanokat kiszurni?

-- Add meg egy parokat (kulcs-ertek) tartalmazo lista elemei kozul az 1-eshez tartozo ertekeket!
ones :: [(Int, a)] -> [a]
ones xs = undefined

-- mappal es filterrel?
ones' :: [(Int, a)] -> [a]
ones' xs = undefined

-- adatbazisok: listak + n-esek kombinacioja
-- Az alabbi adatbazis egy hasznaltauto-kereskedes adatbazisa.
-- Minden markahoz fel vannak sorolva az elerheto tipusok, uzemanyag tipussal, es egy szammal
-- amely azt mondja meg, hogy hany darab van keszleten
database =
  [ ("KIA", [("Optima", "plug-in", 5), ("Soul", "electric", 2), ("CEE'D", "plug-in", 2), ("CEE'D", "gasoline", 12), ("XCEE'D", "plug-in", 3)])
  , ("Renault", [("Zoe", "electric", 4), ("Megane", "plug-in", 2), ("Megane", "gasoline", 5), ("Captur", "plug-in", 1)])
  , ("Nissan", [("Leaf", "electric", 6), ("Qashqai", "mild-hybrid", 1), ("X-Trail", "gasoline", 2)])
  , ("Opel", [("Corsa", "gasoline", 2), ("Corsa", "electric", 3)])
  , ("Toyota", [("RAV4", "hybrid", 4), ("Camry", "hybrid", 2), ("Corolla", "hybrid", 9), ("Prius", "plug-in", 3)])
  , ("Ssangyong", [("Korando", "diesel", 4), ("Rexton", "diesel", 2)])
  , ("Mazda", [("MX-30", "electric", 1), ("3", "mild-hybrid", 2), ("6", "gasoline", 1)])
  ]
-- mi a fenti adatbazis tipusa?

-- Komplex feladat: Add meg, hogy hany darab elektromos autot arulnak a kereskedesben!
electrics xs = undefined

