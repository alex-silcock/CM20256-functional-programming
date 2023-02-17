

ladies    = ["Mary","Kitty","Lydia","Elizabeth","Jane"]
gentlemen = ["Charles","Fitzwilliam","George","William"]

------------------------- Exercise 1

member :: [String] -> String -> Bool
member    []  _ = False
member (x:xs) y
    | x == y    = True
    | otherwise = member xs y

member' :: [String] -> String -> Bool
member'    []  _ = False  
member' (x:xs) y = x == y || member' xs y

remove :: [String] -> String -> [String]
remove [] _ = []
remove (x:xs) y
    | x == y = remove xs y
    | otherwise = x : remove xs y


------------------------- Exercise 2

members :: [String] -> [String] -> Bool
members xs    []  = True
members xs (y:ys)
    | member xs y = members xs ys
    | otherwise = False


members' :: [String] -> [String] -> Bool
members' xs [] = True
members' xs (y:ys) = member xs y && members xs ys


removeAll :: [String] -> [String] -> [String]
removeAll xs [] = xs
removeAll (x:xs) (y:ys) = removeAll (remove (x:xs) y) ys


------------------------- Exercise 3

before :: [Char] -> [Char] -> Bool
before _ [] = undefined
before [] _ = undefined
before (x:xs) (y:ys) = undefined

before' :: [Char] -> [Char] -> Bool
before' = undefined

sorted :: [String] -> Bool
sorted = undefined
