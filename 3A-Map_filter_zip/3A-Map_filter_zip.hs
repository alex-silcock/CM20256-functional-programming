
------------------------- Exercise 1

doubles :: [Int] -> [Int]
doubles = map double
    where double x = x*2

doubles' :: [Int] -> [Int]
doubles' []     = []
doubles' (x:xs) = x*2 : doubles' xs

odds :: [Int] -> [Int]
odds = filter odd

odds' :: [Int] -> [Int]
odds' [] = []
odds' (x:xs)
    | odd x     = x : odds' xs
    | otherwise = odds' xs

doubleodds :: [Int] -> [Int]
doubleodds (x:xs) = doubles (odds (x:xs))

doubleodds' :: [Int] -> [Int]
doubleodds' []  = []
doubleodds' (x:xs)
    | odd x     = x*2 : doubleodds' xs
    | otherwise = doubleodds' xs

------------------------- Exercise 2

shorts :: [String] -> [String]
shorts = undefined

squarePositives :: [Int] -> [Int]
squarePositives = undefined

oddLengthSums :: [[Int]] -> [Int]
oddLengthSums = undefined


------------------------- Exercise 3

remove :: Eq a => [a] -> a -> [a]
remove = undefined

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll = undefined

numbered :: [a] -> [(Int,a)]
numbered = undefined

everyother :: [a] -> [a]
everyother = undefined

same :: Eq a => [a] -> [a] -> [Int]
same = undefined
