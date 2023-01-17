{-# LANGUAGE InstanceSigs #-}
-------------------------------
-- Rekurziv adattipusok: fak --
-------------------------------

addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes (Just x) (Just y) = Just (x + y)
addMaybes _ _ = Nothing

-- motivacio Halado Haskellhez
addMaybes' x y = (+) <$> x <*> y

-- Emlekezteto: listak is rekurziv ADT-k

data MyList a = Nil | Cons a (MyList a) deriving Show

myHead :: MyList a -> Maybe a
myHead Nil = Nothing
myHead (Cons x xs) = Just x


-- Fa tipus, amely leveleiben es csucsaiban is tartalmaz ertekeket
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)

-- data Tree a = Leaf   | Node (Tree a) a (Tree a) -- <- csak a csucsokban tarol erteket
-- data Tree a = Leaf a | Node (Tree a)   (Tree a) -- <- csak a levelekben tarol erteket
-- data Tree a b = Leaf a | Node (Tree a b) b (Tree a b) -- <- csucsokban `b`, levelekben `a` tipusu ertekeket tarol
myTree :: Tree Int
myTree = Node (Node (Leaf 1) 3 (Leaf 2)) 7 (Node (Leaf 4) 6 (Leaf 5))
{-
         7
       /   \
      3     6
     / \   / \
    1   2 4   5
-}

myTree2 :: Tree Int
myTree2 = Node (Node (Leaf 2) 4 (Leaf 3)) 10 (Leaf 2)
{-
         10
       /   \
      4     2
     / \ 
    2   3  
-}

height :: Tree a -> Int
height (Leaf _) = 0
height (Node l _ r) = 1 + (height l `max` height r)

-- transzformalj egy Int-eket tartalmazo listat ugy, hogy minden tartolt erteket megduplazol!
doubleTree :: Tree Int -> Tree Int
doubleTree (Leaf x)     = Leaf $ 2 * x
doubleTree (Node l x r) = Node (doubleTree l) (2 * x) (doubleTree r)

-- Functor tipusosztaly: tetszoleges kontener adattipus elemeit lehet transzformalni
--   Korabban vettuk: map fuggveny listakra
instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x)     = Leaf $ f x
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- add meg a doubleTree fuggvenyt fmap segitsegevel!
doubleTree' t = fmap (2*) t   -- (2*) <$> t

-- Elozo oran vettuk a kovetkezo ket fuggvenyt:
sumTree :: Num a => Tree a -> a
sumTree (Leaf x)     = x
sumTree (Node l x r) = sumTree l + x + sumTree r

makeList :: Tree a -> [a]
makeList (Leaf x)     = [x]
makeList (Node l x r) = makeList l ++ [x] ++ makeList r

myStringTree = Node (Node (Leaf "This ") "is " (Leaf "an ")) "inorder " (Leaf "traversal")

-- Adj meg egy fuggvenyt, ami konkatenalja a faban tarolt listakat
concatTree :: Tree [a] -> [a]
concatTree (Leaf xs)     = xs
concatTree (Node l xs r) = concatTree l ++ xs ++ concatTree r

-- Hajtogatas tetszoleges kontener tipusra: Foldable tipusosztaly
--   Korabban vettuk: foldr fuggveny listakra
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f d (Leaf x) = f x d 
  foldr f d (Node l x r) = foldr f (f x (foldr f d r)) l

-- Miert jo ezt megadni a sajat fa tipusunkra?
-- Nezzuk meg, milyen fuggvenyeket tartalmaz a Foldable:
--   :i Foldable


-- Hogyan lehetne a korabbi fuggenyeket megadni a Foldable peldany segitsegevel? Egyaltalan meg kell adni oket?
concatTree' :: Foldable t => t [a] -> [a]
concatTree' = foldr (++) []

