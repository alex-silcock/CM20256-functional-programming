
ladies    = ["Mary","Kitty","Lydia","Elizabeth","Jane"]
gentlemen = ["Charles","Fitzwilliam","George","William"]
couples   = [("Elizabeth","Fitzwilliam"),("Charlotte","William"),("Lydia","George"),("Jane","Charles")]

------------------------- Exercise 1

ditch :: Int -> [a] -> [a]
ditch _ [] = []
ditch n (x:xs)
  | n > 1     = ditch (n-1) xs
  | otherwise = xs

at :: [a] -> Int -> a
at (x:xs) n
  | n < 0     = error "n must be positive"
  | n == 0    = x
  | otherwise = at xs (n-1)

------------------------- Exercise 2

find :: Eq a => a -> [(a,b)] -> b
find s []     = error "element not found"
find s ((x,y):xs)
  | s == x    = y
  | otherwise = find s xs

which :: Eq a => a -> [a] -> Int
which s (x:xs)    = aux 0 s (x:xs)
  where
    aux :: Eq a => Int -> a -> [a] -> Int
    aux _ s []    = error "empty list"
    aux index s (x:xs)
      | s == x    = index
      | otherwise = aux (index+1) s xs

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = (x<=y) && sorted (y:xs)

------------------------- Exercise 2b
-- Making member, remove and before from Tutorial 2A
-- working for as many lists as possible
-- Eq is for types of equality testing
-- Ord is for types that have an ordering

member :: Eq a => [a] -> a -> Bool
member    []  _ = False
member (x:xs) y
    | x == y    = True
    | otherwise = member xs y

remove :: Eq a => [a] -> a -> [a]
remove [] _     = []
remove (x:xs) y
    | x == y    = remove xs y
    | otherwise = x : remove xs y

before :: Ord a => [a] -> [a] -> Bool
before [] _     = False
before _ []     = True
before (x:xs) (y:ys)
    | x < y     = True
    | x == y    = before xs ys
    | otherwise = False

------------------------- Exercise 3

merge :: Ord a => [a] -> [a] -> [a]
merge xs []   = xs
merge [] ys   = ys
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | x == y    = x : merge xs ys
  | otherwise = y : merge (x:xs) ys

minus :: Ord a => [a] -> [a] -> [a]
minus xs []         = xs
minus [] _          = []
minus (x:xs) (y:ys)
  | x `elem` (y:ys) = minus xs ys
  | otherwise       = x : minus xs (y:ys)

msort :: Ord a => [a] -> [a]
msort []      = []
msort (x:xs)
  | null xs   = [x]
  | otherwise = merge (msort (take (length (x:xs) `div` 2) (x:xs))) (msort (drop (length (x:xs) `div` 2) (x:xs)))