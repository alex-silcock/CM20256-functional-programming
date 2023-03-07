------------------------- Exercise 1

doubles :: [Int] -> [Int]
doubles xs = [ 2 * n | n <- xs ]

odds :: [Int] -> [Int]
odds xs = [ n | n <- xs, odd n ]

doubleodds :: [Int] -> [Int]
doubleodds xs = [ 2 * n | n <- xs, odd n ]

shorts :: [String] -> [String]
shorts xs = [ n | n <- xs, length n <= 5 ]

squarePositives :: [Int] -> [Int]
squarePositives xs = [ n * n | n <- xs, n > 0 ]

oddLengthSums :: [[Int]] -> [Int]
oddLengthSums xs = [ sum n | n <- xs, odd (length n) ]

remove :: Eq a => [a] -> a -> [a]
remove xs x = [ n | n <- xs, n /= x]

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll xs ys = [ n | n <- xs, n `notElem` xs || n `notElem` ys]

everyother :: [a] -> [a]
everyother xs = [ snd n | n <- zip [1..] xs, odd (fst n)]

same :: Eq a => [a] -> [a] -> [Int]
same xs ys = [ fst n | n <- zip [1..] (zipWith (==) xs ys), snd n ]

same' :: Eq a => [a] -> [a] -> [Int]
same' xs ys = [ first n | n <- zip3 [1..] xs ys, second n == third n ]
    where
        first (a, _, _) = a
        second (_, a, _) = a
        third (_, _, a) = a

------------------------- Exercise 2

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [(a, b) | a <- xs, b <- ys]

applies :: [a -> b] -> [a] -> [b]
applies fs xs = [f x | f <- fs, x <- xs]

selfpairs :: [a] -> [(a,a)]
selfpairs xs = [(xs !! a, b) | (a, x) <- zip [0..] xs, b <- drop a xs]

pyts :: Int -> [(Int,Int,Int)]
pyts n = [(a, b, c) | c <- [1..n], b <- [1..c-1], a <- [1..b-1], a^2 + b^2 == c^2]