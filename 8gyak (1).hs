import Data.Char
import Data.List

-- @ minta (alias):
f l@(x:xs) = l

-- Add meg egy parokat (kulcs-ertek) tartalmazo lista elemei kozul az 1-eshez tartozo ertekeket!
ones :: [(Int, a)] -> [a]
ones xs = [snd x | x <- xs, fst x == 1]

ones2 xs = [ y | (x, y) <- xs, x == 1]
ones3 xs = [ y | (1, y) <- xs]

-- mappal es filterrel?
ones' :: [(Int, a)] -> [a]
ones' xs = map snd (filter (\x -> fst x == 1) xs) -- filter (\(x, _) -> x == 1) xs

ones'' xs = map snd $ filter (\x -> fst x == 1) xs

-- (f . g) x = f (g x) 
ones''' = map snd . filter (\x -> fst x == 1)

-- Dontsd el egy szamrol, hogy prim-e?
isPrime :: Integer -> Bool
isPrime n = null [x | x <- [2..n `div` 2], n `mod` x == 0]

-- Add meg az osszes primszamot!
primes :: [Integer]
primes = [n | n <- [2..], isPrime n]

-- gyujtsd ki egy lista minden 3. elemet!
everyThird :: [a] -> [a]
everyThird xs = [snd x | x <- zip [1..] xs, fst x `mod` 3 == 0]


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

-- Komplex feladat: Add meg, hogy hany darab KIA elektromos autot arulnak a kereskedesben!
electrics xs = sum [count | (brand, cars) <- xs, (_, fuel, count) <- cars, fuel == "electric", brand == "KIA"]


---------------------------
-- AGGREGACIO/HAJTOGATAS --
---------------------------

-- Add meg egy lista összegét!
summ :: Num a => [a] -> a
summ []     = 0
summ (x:xs) = x + summ xs

-- Add meg egy lista paros elemeinek osszeget!
sumEvens :: [Int] -> Int
sumEvens [] = 0
sumEvens (x:xs) = sumEvens xs + if even x
                                then x
                                else 0

-- Alternativ javaslat filterrel? - fuggvenykompozicio
sumEvens2 = undefined

-- Valogass szet egy listat az elemek paritasa szerint!
oddEven :: [Int] -> ([Int], [Int])
oddEven [] = ([], [])
oddEven (x:xs) = case oddEven xs of
                    (ys, zs) -> if even x
                                then (x:ys, zs)
                                else (ys, x:zs)

myfoldr = undefined

summ' = undefined

sumEvens' = undefined

oddEven' = undefined

electrics' xs = undefined


