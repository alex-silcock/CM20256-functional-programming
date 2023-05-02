import Data.Char ( isUpper, toLower, toUpper, isAlpha )
import Data.List ( sort )

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


add3 :: [(Int, String)] -> [(Int, String)]
add3 = map (\(x, y) -> (x + 3, y))

getRidInt :: [(Int, String)] -> [String]
getRidInt = map snd

list1 :: [Int]
list1 = [8, 12, 9, 6, 4, 39, 65, 22, 21]

list2 :: [Int]
list2 = [5, 9, 3, 46, 1, 10, 200, 30, 99]

evens :: [Int] -> [Int]
evens = filter even



addLists :: [Int] -> [Int] -> [Int]
addLists xs ys = map plus (zip xs ys)
    where
        plus :: (Int, Int) -> Int
        plus (x, y) = x + y

addLists2 :: [Int] -> [Int] -> [Int]
addLists2 = zipWith (+)

removeExclamationMarks :: String -> String
removeExclamationMarks = filter (/= '!')

smallEnough :: [Int] -> Int -> Bool
smallEnough xs v = and [x <= v | x <- xs]

solve :: String -> String
solve str
  | upperLower str 0 0 > 0 = map toUpper str
  | otherwise              = map toLower str
  where
    upperLower :: String -> Int -> Int -> Int
    upperLower "" upper lower = upper - lower
    upperLower (x:xs) upper lower
      | isUpper x = upperLower xs (upper+1) lower
      | otherwise = upperLower xs upper (lower+1)

xo :: String -> Bool
xo str = xs == os
  where
    xs = length $ filter (\x -> x == 'x' || x == 'X') str
    os = length $ filter (\x -> x == 'o' || x == 'O') str

xo2 :: String -> Bool
xo2 str = xs == os
  where
    xs = length $ filter (`elem` "xX") str
    os = length $ filter (`elem` "oO") str

reverseLetter :: String -> String
reverseLetter = reverse . filter isAlpha

getEvenNumbers :: [Int] -> [Int]
getEvenNumbers = filter even

flatSort :: [[Int]] -> [Int]
flatSort = sort . concat

sumList :: [Int] -> Int
sumList = foldr (+) 0

allTrue :: [Bool] -> Bool
allTrue = foldr (&&) True

or1 :: [Bool] -> Bool
or1 = foldr (||) False

foldLength :: String -> Int
foldLength = foldr (\x  -> (+) 1) 0

capitals :: String -> [Int]
capitals xs = [fst x | x <- zip [0..(length xs)] xs, isUpper (snd x)]


