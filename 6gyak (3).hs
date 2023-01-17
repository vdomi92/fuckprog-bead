takedrop :: [a] -> Int -> Int -> [a]
takedrop xs m n = take (n - m + 1) (drop m xs)


-- ures-e egy lista? - null stdlibben
isEmpty :: [a] -> Bool
isEmpty []    = True 
isEmpty (_:_) = False

isEmpty' xs = length xs == 0
-- egyelemu-e egy lista?
-- (x:y:ys)

isSingletonList :: [a] -> Bool
isSingletonList (x:[]) = True -- [x]
isSingletonList _ = False

isSingletonList' [] = False
isSingletonList' (x:xs) = case xs of
                            [] -> True
                            _  -> False

-- ketelemu-e egy lista?
-- (x:y:[])  -- [_, _]

-- stb. Hogyan lehetne megirni azt, hogy 139 elemu-e egy lista?
containsN :: [a] -> Int -> Bool
containsN []     n = n == 0
containsN (x:xs) n = containsN xs (n - 1)

{-
  containsN [1,2] 1 = containsN (1:[2]) 1 =>
  containsN [2] (1 - 1) = containsN (2:[]) (1 - 1) =>
  containsN [] ((1 - 1) - 1) =>
  ((1 - 1) - 1) == 0 =>
  -1 == 0 =>
  False
-}

-- rekurzio: nezzuk meg egyesevel kibontva a beta-redukciokat!
-- szorozd ossze a lista elemeit!
-- mult :: Num a => [a] -> a
-- mult []     = 0
-- mult (x:xs) = x * mult xs

{-
  mult [1,2] => 1 * mult [2] => 1 * (2 * mult []) => 1 * (2 * 0)
-}

mult :: Num a => [a] -> a
mult []     = 1 -- egysegelem
mult (x:xs) = x * mult xs


-- Adj meg egy listat, amely -10-tol 10-ig tartalmazza az egesz szamokat!
-- Intervallumkifejezesek...


-- szorozd meg egy lista paros elemeit kettovel!
double :: [Int] -> [Int]
double []     = []
double (x:xs)
  | even x = 2 * x : double xs
  | odd  x = x     : double xs

double' :: [Int] -> [Int]
double' []     = []
double' (x:xs) = (if even x then 2 * x else x) : double' xs

-- Hany darab paratlan eleme van egy listanak?


-- Add meg egy lista n-edik elemet! (error fuggveny)


-- Add meg az osszefesulo rendezes osszefesulo lepeset (merge fuggveny)!
-- Ebben a lepesben ket listat kell rendezetten osszefesulni.


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


