-- importok:
import Data.Char -- isLower, isUpper
import Data.List -- sort

-----------------------------------------------------------------

-- lista kifejezesek, literalok


-- listak tipusa, elemek tipusa, stringek
-- [[]] :: [[a]]
-- [True, [False]] <- tipushiba
-- [[], [[]]] :: [[[a]]]
-- [[(True, 'a')], [(True, 'a'), (False, 'b')]] :: [[(Bool, Char)]]
-- ([], []) :: ([a], [b])


-- beepitett fuggvenyek hasznalata listakra
-- ++, reverse, nub, sort, take, drop, length, !!
-- Hoogle tipusokkal


-- hany fajta listat ismersz?
-- ket lista van:
-- ures lista - []
--                   v------ elso elem/fejelem
-- nem ures lista - (x:xs)
--                      ^------ folytatas

hd :: [a] -> a
hd []     = error "Empty list"
hd (x:xs) = x

tl :: [a] -> [a]
tl []     = error "Empty list"
tl (x:xs) = xs


-- ures-e egy lista?

-- egyelemu-e egy lista?
-- (x:y:ys)
isSingletonList = undefined

-- ketelemu-e egy lista?


-- stb. Hogyan lehetne megirni azt, hogy 139 elemu-e egy lista?



-- rekurzio: nezzuk meg egyesevel kibontva a beta-redukciokat!
-- szorozd ossze a lista elemeit!


-- Adj meg egy listat, amely -10-tol 10-ig tartalmazza az egesz szamokat!


-- szorozd meg egy lista paros elemeit kettovel!


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


