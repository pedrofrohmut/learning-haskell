-- Problem 01
myLast :: [a] -> a
myLast (x:xs)
    | null xs = x
    | otherwise = myLast xs

myButLast :: [a] -> a
myButLast (x1:x2:xs)
    | null xs = x1
    | otherwise = myButLast (x2:xs)
