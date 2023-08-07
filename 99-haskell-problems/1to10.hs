-- 01. Last elem of a list
myLast :: [a] -> a
myLast (x:xs)
    | null xs = x
    | otherwise = myLast xs

-- 02. Second last elem of a list
myButLast :: [a] -> a
myButLast (x1:x2:xs)
    | null xs = x1
    | otherwise = myButLast (x2:xs)

-- 03. Nth elem of a list
elemAt :: [a] -> Int -> a
elemAt (x:xs) n
    | n == 0 = x
    | otherwise = elemAt xs (n - 1)
