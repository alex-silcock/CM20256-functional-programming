
import Data.Char ( ord, isAlpha, isUpper, toUpper )

------------------------- a

allTrue :: [Bool] -> Bool
allTrue []      = True
allTrue (x:xs)
    | x         = allTrue xs
    | otherwise = False

allTrue' :: [Bool] -> Bool
allTrue' = and

allTrue'' :: [Bool] -> Bool
allTrue'' = foldr (&&) True

------------------------- b

ws :: [String]
ws = ["Smoke","me","a","kipper","I'll","be","back","for","breakfast"]

longestLength :: [[a]] -> Int
longestLength []     = 0
longestLength (x:xs) = max (length x) (longestLength xs)

longestLength' :: [[a]] -> Int
longestLength' xs = maximum [length x | x <- xs]

longestLength'' :: [[a]] -> Int
longestLength'' = foldr (max . length) 0

------------------------- c

sumOddSquares :: [Int] -> Int
sumOddSquares [] = 0
sumOddSquares (x:xs)
    | odd x      = (x*x) + sumOddSquares xs
    | otherwise  = sumOddSquares xs

sumOddSquares' :: [Int] -> Int
sumOddSquares' xs = sum [x*x | x <- xs, odd x]

sumOddSquares'' :: [Int] -> Int
sumOddSquares'' xs = sum (map (^2) (filter odd xs))

------------------------- d
fs :: [String]
fs = ["Fish","for","breakfast","??"]

shortFWords :: [String] -> Bool
shortFWords [] = False
shortFWords (x:xs)
    | length x == 4 && head x == 'F' = True
    | otherwise                      = shortFWords xs

shortFWords' :: [String] -> Bool
shortFWords' xs = 'F' `elem` ([head x | x <- xs])

shortFWords'' :: [String] -> Bool
shortFWords'' xs = any ((== 'F') . head) (filter (\x -> not (null x) && isUpper (head x)) xs)

------------------------- e

wordScore :: String -> Int
wordScore ""     = 0
wordScore (x:xs)
    | isAlpha x  = subtract 64 (ord (toUpper x)) + wordScore xs
    | otherwise  = wordScore xs

wordScore' :: String -> Int
wordScore' xs = sum [subtract 64 (ord (toUpper x)) | x <- filter isAlpha xs]

wordScore'' :: String -> Int
wordScore'' = foldr charScore 0 . filter isAlpha
  where
    charScore c total = subtract 64 (ord (toUpper c)) + total

------------------------- f

concatCheapWords :: [String] -> String
concatCheapWords [] = ""
concatCheapWords (x:xs)
    | wordScore'' x <= 42 = " " ++ x ++ concatCheapWords xs
    | otherwise           = concatCheapWords xs

concatCheapWords' :: [String] -> String
concatCheapWords' xs = " " ++ unwords [x | x <- xs, wordScore'' x <= 42]

concatCheapWords'' :: [String] -> String
concatCheapWords'' = foldr isCheapWord ""
  where
    isCheapWord x result
      | wordScore'' x <= 42 = " " ++ x ++ result
      | otherwise           = result