-- Problem 01
myLast :: [a] -> a
myLast (x:xs)
    | null xs = x
    | otherwise = myLast xs
