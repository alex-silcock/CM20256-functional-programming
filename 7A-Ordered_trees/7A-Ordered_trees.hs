{-
data IntTree = Empty | Node Int IntTree IntTree

t :: IntTree
t = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 5 Empty (Node 6 Empty Empty))

instance Show IntTree where
    show = unlines . aux ' ' ' '
      where
        aux _ _ Empty = []
        aux c d (Node x s t) = 
          [ c:' ':m | m <- aux ' ' '|' s ] ++ 
          ['+':'-':show x] ++ 
          [ d:' ':n | n <- aux '|' ' ' t ]
-}
isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _     = False

------------------------- Exercise 1

member :: Ord a => a -> Tree a -> Bool
member x Empty = False
member x (Node y left right)
  | x == y     = True
  | x < y      = member x left
  | otherwise  = member x right

largest :: Tree a -> a
largest Empty            = error "Can't find the max of an empty tree"
largest (Node x l Empty) = x
largest (Node x l r)     = largest r

ordered :: Ord a => Tree a -> Bool
ordered = isOrdered . flatten
  where
    flatten :: Tree a -> [a]
    flatten Empty               = []
    flatten (Node x left right) = flatten left ++ [x]
                                          ++ flatten right
    isOrdered :: Ord a => [a] -> Bool
    isOrdered []  = True
    isOrdered [x] = True
    isOrdered (x:y:xs)
      | x > y     = False
      | otherwise = isOrdered (y:xs)

ordered' :: Ord a => Tree a -> Bool
ordered' Empty = True
ordered' (Node x l r) = (largest l <= x && ordered' l) && (smallest r >= x && ordered' r)
  where
    smallest :: Tree a -> a
    smallest Empty            = error "Cant't find smallest of an empty tree"
    smallest (Node x Empty r) = x
    smallest (Node x l r)     = smallest l

deleteLargest :: Tree a -> Tree a
deleteLargest Empty            = Empty
deleteLargest (Node x l Empty) = l
deleteLargest (Node x l r)     = Node x l (deleteLargest r)

delete :: Ord a => a -> Tree a -> Tree a
delete _ Empty  = undefined
delete y (Node x l r)
    | y < x     = Node x (delete y l) r
    | y > x     = Node x l (delete y r)
    | isEmpty l = r
    | otherwise = Node (largest l) (deleteLargest l) r

------------------------- Exercise 2

data Tree a = Empty | Node a (Tree a) (Tree a)

t :: Tree Int
t = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 5 Empty (Node 6 Empty Empty))

instance Show a => Show (Tree a) where
    show = unlines . aux ' ' ' '
      where
        aux _ _ Empty = []
        aux c d (Node x s t) = 
          [ c:' ':m | m <- aux ' ' '|' s ] ++ 
          ['+':'-':show x] ++ 
          [ d:' ':n | n <- aux '|' ' ' t ]

------------------------- Lambda-calculus

type Var = String

data Term =
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term

example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Apply (Variable "a") (Variable "c"))) (Variable "x")) (Variable "b")))

pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m

instance Show Term where
  show = pretty

n1 :: Term
n1 = Lambda "x" (Variable "x")

n2 :: Term
n2 = Lambda "x" (Apply (Lambda "y" (Variable "x")) (Variable "z"))

n3 :: Term
n3 = Apply (Lambda "x" (Lambda "y" (Apply (Variable "x") (Variable "y")))) n1

-------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

minus :: Ord a => [a] -> [a] -> [a]
minus xs [] = xs
minus [] ys = []
minus (x:xs) (y:ys)
    | x <  y    = x : minus    xs (y:ys)
    | x == y    =     minus    xs    ys
    | otherwise =     minus (x:xs)   ys

-------------------------

used :: Term -> [Var]
used (Variable x) = [x]
used (Lambda x n) = [x] `merge` used n
used (Apply  n m) = used n `merge` used m

free :: Term -> [Var]
free (Variable x) = [x]
free (Lambda x n) = free n `minus` [x]
free (Apply  n m) = free n `merge` free m


------------------------- Exercise 3

numeral :: Int -> Term
numeral i = Lambda "f" (Lambda "x" (numeral' i))
  where
    numeral' i
      | i <= 0    = Variable "x"
      | otherwise = Apply (Variable "f") (numeral' (i-1))

------------------------- Exercise 4

variables :: [Var]
variables = [ [x] | x <- ['a'..'z'] ] ++ [ x : show i | i <- [1..] , x <- ['a'..'z'] ]

removeAll :: [Var] -> [Var] -> [Var]
removeAll xs ys = [ x | x <- xs , x `notElem` ys ]

fresh :: [Var] -> Var
fresh = head . removeAll variables


rename :: Var -> Var -> Term -> Term
rename x y (Variable z)
    | z == x    = Variable y
    | otherwise = Variable z
rename x y (Lambda z n)
    | z == x    = Lambda z n
    | otherwise = Lambda z (rename x y n)
rename x y (Apply n m) = Apply (rename x y n) (rename x y m)


substitute :: Var -> Term -> Term -> Term
substitute x n (Variable y)
    | x == y    = n
    | otherwise = Variable y
substitute x n (Lambda y m)
    | x == y    = Lambda y m
    | otherwise = Lambda z (substitute x n (rename y z m))
    where z = fresh (used n `merge` used m `merge` [x,y])
substitute x n (Apply m p) = Apply (substitute x n m) (substitute x n p)