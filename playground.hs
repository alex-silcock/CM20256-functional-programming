import Data.Char ( isUpper, toLower, toUpper, isAlpha, isDigit, digitToInt )
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

getNumberFromString :: String -> Int
getNumberFromString x = read $ filter (\x -> x `elem` ['0'..'9']) x

listOf :: Int -> [Int]
listOf n = filter even [1..n]

doubleComp :: [Int] -> [Int]
doubleComp xs = [x*2 | x <- xs]

getCount :: String -> Int
getCount = foldr (\c acc -> if c `elem` "aeiou" then acc + 1 else acc) 0

getEvenNums :: [Int] -> [Int]
getEvenNums xs = [x | x <- xs, even x]

getVowels :: String -> String
getVowels str = [s | s <- str, s `elem` "aeiouAEIOU"]

sumEvenNums :: [Int] -> Int
sumEvenNums xs = sum [x | x <- xs, even x]

getCommonElems :: [Int] -> [Int] -> [Int]
getCommonElems str1 str2 = [c1 | c1 <- str1, c2 <- str2, c1 == c2]

getLongStrings :: [String] -> [String]
getLongStrings xs = [x | x <- xs, length x > 5]

sumDigits :: String -> Int
sumDigits "" = 0
sumDigits (s:ss)
  | isDigit s = digitToInt s + sumDigits ss
  | otherwise = sumDigits ss

sumDigits2 :: String -> Int
sumDigits2 str = sum [digitToInt c | c <- str, isDigit c]

sumDigits3 :: String -> Int
sumDigits3 str = foldr ((+) . digitToInt) 0 (filter isDigit str)

shortLengths :: [String] -> Int
shortLengths xs = foldr (+) (0) (filter (<=5) (map length xs))

shorts :: [String] -> [String]
shorts = filter (\s -> length s <= 5)

addToEnd :: Int -> [Int] -> [Int]
addToEnd = foldr (:) . (:[])

rev :: [Int] -> [Int]
rev = foldr addToEnd []

s :: [Int] -> Int
s = foldl (-) 0

s2 :: [Int] -> Int
s2 = foldr (-) 0

thatlist :: [Int]
thatlist = map f (filter even [0..10])
  where f n = 3*n + 1

stillthatlist :: [Int]
stillthatlist = map (\s -> 3*s + 1) (filter even [0..10])

thisisstillthatlist :: [Int]
thisisstillthatlist = [3*n + 1 | n <- [0..10], even n]

reverseShorts :: [String] -> [String]
reverseShorts [] = []
reverseShorts (x:xs)
  | length x <= 5 = reverseShorts xs ++ [x]
  | otherwise     = reverseShorts xs

reverseShorts2 :: [String] -> [String]
reverseShorts2 xs = reverse [x | x <- xs, length x <= 5]

reverseShorts3 :: [String] -> [String]
reverseShorts3 = reverse . filter f
  where f x = length x <= 5

reverseShorts4 :: [String] -> [String]
reverseShorts4 = reverse . filter (\x -> length x <= 5)

sayHello :: IO String
sayHello =
  do
    putStrLn "Hello"
    getLine

count :: Int -> IO ()
count x = do
  print x
  if x == 0
  then do
    putStrLn "ZERO"
  else do
    count (x-1)

afact :: Int -> Int -> Int
afact a 0 = a
afact a n = afact (a*n) (n-1)

areverse :: [Int] -> [Int] -> [Int]
areverse a []     = a
areverse a (x:xs) = areverse (x:a) xs

sL :: [String] -> Int
sL = foldr (+) 0 . map length . filter (\x -> length x <= 5)

sumDigits'' :: String -> Int
sumDigits'' xs = sum [digitToInt x | x <- xs, isDigit x]

snoc :: Int -> [Int] -> [Int]
snoc = foldr (:) . (:[])