------------------------- Exercise 1

square :: Int -> Int
square x = x * x

pythagoras :: (Eq a, Num a) => a -> a -> a -> Bool
pythagoras a b c = a*a + b*b == c*c

------------------------- Exercise 2

factorial :: Int -> Int
factorial x
    | x <= 1    = 1
    | otherwise = x * factorial (x-1)

euclid :: Int -> Int -> Int
euclid x y
    | x < 1 || y < 1  = 0
    | x == y          = x
    | x <  y          = euclid x (y-x)
    | x >  y          = euclid y (x-y)

power :: Int -> Int -> Int
power x y
    | y < 0  = power (1 `div` x) (-y)
    | y == 0 = 1
    | even y = power (square x) (y `div` 2)
    | odd y  = x * power (square x) ((y-1) `div` 2)

------------------------- Exercise 3

range :: Int -> Int -> [Int]
range x y = [x..y]

times :: [Int] -> Int
times = product

fact :: Int -> Int
fact x = times (range 1 x)