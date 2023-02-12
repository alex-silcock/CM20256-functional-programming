countdown :: Int -> [Int]
countdown i
    | i < 0 = []
    | otherwise = i : countdown (i-1)

total :: [Int] -> Int
total [] = 0
total (x:xs) = x + total xs

goodlength :: [Int] -> Int
goodlength [] = 0
goodlength (x:xs) = 1 + goodlength xs

-- Using wildcard _
gl2 :: [Int] -> Int
gl2 [] = 0
gl2 (_:xs) = 1 + gl2 xs

grab :: Int -> [Int] -> [Int]
grab _ [] = []
grab n (x:xs)
    | n <= 0 = []
    | otherwise = x : grab (n-1) xs

-- fst (x, y) returns the first element, x
-- snd (x, y) returns the second element, y

swap :: (Int, String) -> (String, Int)
swap (x, y) = (y, x)

-- Invert two elements in a list of pairs
listOfPairs :: [(Int, String)]
listOfPairs =
    [(100, "Hello")
    ,(200, "World")
    ,(300, "Haskell") 
    ]

-- Using !! for indexing, e.g. listOfPairs!!2 returns (300, "Haskell")

-- Invert each element in a list of pairs
invert :: [(Int, String)] -> [(String, Int)]
invert [] = []
invert ((x,y):xs) = (y,x) : invert xs

exampleUncurry :: (Int -> Int -> Int) -> (Int, Int) -> Int
exampleUncurry f (x,y) = f x y

exampleCurry :: ((Int,Int) -> Int) -> Int -> Int -> Int
exampleCurry f x y = f (x,y)