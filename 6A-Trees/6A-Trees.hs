data IntTree = Empty | Node Int IntTree IntTree
  -- deriving Show

t :: IntTree
t = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 5 Empty (Node 6 Empty Empty))


------------------------- Exercise 1

isEmpty :: IntTree -> Bool
isEmpty Empty = True
isEmpty _     = False

rootValue :: IntTree -> Int
rootValue Empty        = 0
rootValue (Node i _ _) = i

height :: IntTree -> Int
height Empty               = 0
height (Node x left right) = 1 + max (height left) (height right)

member :: Int -> IntTree -> Bool
member x Empty = False
member x (Node y left right)
  | x == y     = True
  | x < y      = member x left
  | otherwise  = member x right

paths :: Int -> IntTree -> [[Int]]
paths x Empty = []
paths x (Node y left right)
  | x == y    = [[]] ++ map (0 :) (paths x left) ++ map (1 :) (paths y right)
  | otherwise =         map (0 :) (paths x left) ++ map (1 :) (paths x right)

-------------------------

instance Show IntTree where
    show = unlines . aux ' ' ' '
      where
        aux _ _ Empty = []
        aux c d (Node x s t) =
          [ c:' ':m | m <- aux ' ' '|' s ] ++
          ['+':'-':show x] ++
          [ d:' ':n | n <- aux '|' ' ' t ]

------------------------- Exercise 2

type Var = String

data Term =
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term
  -- deriving Show

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
n3 = Apply (Lambda "x" (Lambda "y" (Apply (Variable "x") (Variable "y")))) (Lambda "x" (Variable "x"))

used :: Term -> [Var]
used (Variable x) = [x]
used (Lambda x m) = [x] `merge` used m
used (Apply  n m) = used n `merge` used m

free :: Term -> [Var]
free (Variable x) = [x]
free (Lambda x m) = free m `minus` [x]
free (Apply  m n) = free m `merge` free n



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
