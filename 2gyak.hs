-- TMS: tms.inf.elte.hu
-- Fajl/modul betoltese -> :l(oad)
-- Modul ujratoltes     -> :r(eload)

-- Binaris, hexadecimalis, oktalis literalok
-- :set -XBinaryLiterals

-- Definialj egyszeru fuggvenyeket: six, tobb tipussal
six :: Num p => p
six = 6

six' :: Integer
six' = 6

six'' :: Double
six'' = 6

-- Definiald a double fuggvenyt!
double :: Double -> Double
double x = x * 2

double' :: Num t => t -> t
double' x = x * 2


-- Definiald az even fuggvenyt! Lehetseges-e? Meg tudod hivni?
even' :: Int -> Bool
even' x = x `mod` 2 == 0  -- mod x 2 == 0

eVen :: Int -> Bool
eVen x = True

-- logikai fuggvenyek
-- Megszerkesztheto-e egy haromszog a harom oldala alapjan?

isTriangle :: Double -> Double -> Double -> Bool
isTriangle x y z = x + y > z && x + z > y && y + z > x

-- Pitagoraszi szamharmas-e a 3 megadott parameter?

-- Szorzat tipus: rendezett n-es (~~~~ rekordok)
-- Adj ossze ket racionalis szamot!

-- rac szam = szamlalo / nevezo
addRac :: (Int, Int) -> (Int, Int) -> (Int, Int)
-- addRac x y = (fst x * snd y + fst y * snd x, snd x * snd y)
addRac (x1, x2) (y1, y2) = (x1 * y2 + y1 * x2, x2 * y2)
-- mintaillesztes / pattern matching

-- fromIntegral : egesz -> tort

-- Szorozz ossze ket racionalis szamot!

-- Adott egy helyvektor. Nyujtsd meg konstanszorosara!

-- Szamtipusok kozotti "konverzio"
-- Kerekitesek: round, floor, ceiling, truncate
-- Szamold ki x y-nal vett maradekanak gyoket!

-- Oldd meg az egyutthatoival megadott masodfoku egyenletet!

