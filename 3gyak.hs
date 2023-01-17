distance :: (Double, Double) -> (Double, Double) -> Double
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

distance2 :: (Int, Int) -> (Int, Int) -> Double
distance2 (x1, y1) (x2, y2) = sqrt (fromIntegral ((x2 - x1) ^ 2 + (y2 - y1) ^ 2))

-- Igazabol mar mindent megtanultatok az elozo ket oran a funkcionalis programozasrol:
-- fuggvenyek, fuggvenyalkalmazas, mintaillesztes, tipusok

-- Program vegrehajtasa: egyszerusites (beta-redukciok sorozata)
-- execution  = (+) ((+) 3 2) ((+) 3 4) => ?

{-
  (+) ((+) 3 2) ((+) 3 4) => (+) 5 ((+) 3 4) => (+) 5 7 => 12
-}

------------------------------------------------------------------------
---------------------------- Szorzat tipus -----------------------------
------------------------------------------------------------------------
-- valojaban minden (parameterrel rendelkezo) fuggvenynek 1 parametere van:
-- Curry, uncurry, reszleges fuggvenyalakalmazas
-- peldaul: mod, (+)


-- Oldd meg a masodfoku egyenletet (tfh. van megoldas), add meg mindegyik megoldast
determinant a b c = b ^ 2 - 4 * a * c

solve :: Double -> Double -> Double -> (Double, Double)
solve a b c = ((-b + sqrt (determinant a b c)) / (2 * a), 
               (-b - sqrt (determinant a b c)) / (2 * a))

solve' :: Int -> Int -> Int -> (Double, Double)
solve' a' b' c' = ((-b + sqrt (determinant a b c)) / (2 * a), 
                   (-b - sqrt (determinant a b c)) / (2 * a))
  where
    a = fromIntegral a'
    b = fromIntegral b'
    c = fromIntegral c'

------------------------------------------------------------------------
-------------------------- Mintaillesztés ------------------------------
------------------------------------------------------------------------

-- add meg az is0 fuggvenyt!

is0 :: Int -> Bool
is0 x = x == 0

is0' :: Int -> Bool
is0' x = if x == 0 then True else False

max2 a b c = if a > b
             then
               if b > c
               then b
               else c -- <- ez igy nem jo, HF befejezni


-- "Rendezések"
pythagoreanTriple a b c = undefined

-- add meg a logikai konjunkcio muveletet!
-- if-then-else, case, guardok, mintaillesztes


------------------------------------------------------------------------
--------------------------------- Listák -------------------------------
------------------------------------------------------------------------

-- hany kulonbozo lista van?

-- head

-- tail

-- ures-e?

-- egyelemu-e?

-- Halmazkifejezesek

