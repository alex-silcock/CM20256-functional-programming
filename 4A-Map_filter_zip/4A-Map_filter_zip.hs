
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
shorts = filter lessThan5
    where lessThan5 s = length s <= 5

squarePositives :: [Int] -> [Int]
squarePositives xs = map (^2) (filter (>0) xs)

oddLengthSums :: [[Int]] -> [Int]
oddLengthSums xs = map sum (filter oddLength xs)
    where oddLength ys = odd (length ys)


------------------------- Exercise 3

remove :: Eq a => [a] -> a -> [a]
remove string c = filter (/= c) string

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll str1 str2 = filter (`notElem` str2) str1

numbered :: [a] -> [(Int,a)]
numbered = zip [1..]

everyother :: [a] -> [a]
everyother s = map snd $ filter (odd . fst) $ numbered s

same :: Eq a => [a] -> [a] -> [Int]
same s1 s2 = map fst $ filter snd $ numbered $ zipWith (==) s1 s2