
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
which s (x:xs) = aux 0 s (x:xs)
  where
    aux :: Eq a => Int -> a -> [a] -> Int
    aux _ s [] = error "empty list"
    aux index s (x:xs)
      | s == x = index
      | otherwise = aux (index+1) s xs

sorted :: Ord a => [a] -> Bool
sorted = undefined

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
remove [] _ = []
remove (x:xs) y
    | x == y = remove xs y
    | otherwise = x : remove xs y

before :: Ord a => [a] -> [a] -> Bool
before [] _ = False
before _ [] = True
before (x:xs) (y:ys)
    | x < y = True
    | x == y = before xs ys
    | otherwise = False

------------------------- Exercise 3

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = undefined
merge [] ys = undefined
merge (x:xs) (y:ys) = undefined

minus :: Ord a => [a] -> [a] -> [a]
minus = undefined

msort :: Ord a => [a] -> [a]
msort = undefined
