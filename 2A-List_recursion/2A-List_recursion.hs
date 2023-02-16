

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
members xs    []  = undefined
members xs (y:ys) = undefined

members' :: [String] -> [String] -> Bool
members' = undefined

removeAll :: [String] -> [String] -> [String]
removeAll = undefined


------------------------- Exercise 3

before :: [Char] -> [Char] -> Bool
before _ [] = undefined
before [] _ = undefined
before (x:xs) (y:ys) = undefined

before' :: [Char] -> [Char] -> Bool
before' = undefined

sorted :: [String] -> Bool
sorted = undefined
