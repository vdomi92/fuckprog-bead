

-- ismetles:
inc x = x + 1

x = 0
-- Mi lesz az eredmenye x-nek, ha lefuttatjuk 'inc x'-et?



-- mintaillesztes es tobb egyenlettel megadott fuggvenyek
-- wildcard minta
-- A felsorolas sorrendje!
-- Add meg a logikai 'es'-t

not' :: Bool -> Bool
not' x = if x then False else True

not'' :: Bool -> Bool
not'' False = True
not'' True  = False

and' :: Bool -> Bool -> Bool
and' x y = if x
           then if y
                then True
                else False
           else False

and2 :: Bool -> Bool -> Bool
and2 True  True  = True
and2 True  False = False
and2 False True  = False
and2 False False = False
-- and2 False False = True <- redundans eset

and3 :: Bool -> Bool -> Bool
and3 True True = True
and3 x    y    = False

and4 :: Bool -> Bool -> Bool
and4 True True = True -- FONTOS A SORREND!
and4 _    _    = False

and5 :: Bool -> Bool -> Bool
and5 True x = x
and5 _    _ = False

and6 :: Bool -> Bool -> Bool
and6 x y
  | x == True && y == True = True
  | otherwise              = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _     _     = True

or2 :: Bool -> Bool -> Bool
-- or2 x y = x + y <- Bool tipus =/= Integer
or2 x y = not' (and' (not' x) (not' y))

-- osszeadas kettes szamrendszerben 2 biten:
add2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
add2 = undefined

-- Adj meg egy fuggenyt, amely a sortores karaktert szokozre "csereli"
swapEndline :: Char -> Char
swapEndline x = if x == '\n' then ' ' else x

swapEndline' :: Char -> Char
swapEndline' '\n' = ' '
swapEndline' x    = x

-- szamologep: calc
calc :: Int -> Char -> Int -> Int
calc = undefined


-- osztast is tudo szamologep:
calc' :: Int -> Char -> Int -> Int
calc' = undefined


