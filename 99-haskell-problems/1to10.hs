-- 01. Find the last element of a list
myLast :: [a] -> a
myLast (x:xs)
    | null xs = x
    | otherwise = myLast xs

-- 02. Find the last-but-one (or second-last) element of a list
myButLast :: [a] -> a
myButLast (x1:x2:xs)
    | null xs = x1
    | otherwise = myButLast (x2:xs)

-- 03. Find the K'th element of a list
elemAt :: [a] -> Int -> a
elemAt (x:xs) n
    | n == 0 = x
    | otherwise = elemAt xs (n - 1)

-- 04 Find the number of elements in a list
myLength :: [a] -> Int
myLength list =
    let count [] n = n
        count (y:ys) n = count ys (n + 1)
    in  count list 0

-- 05 Reverse a list
myReverse :: [a] -> [a]
myReverse list =
    let reverseIt result [] = result
        reverseIt result (x:xs) = reverseIt (x : result) xs
    in  reverseIt [] list

-- 06 Find out whether a list is a palindrome
isPolindrome :: Eq a => [a] -> Bool
isPolindrome list = list == (reverse list)

-- 07 Flatten a nested list structure
-- λ> flatten (Elem 5)
-- [5]
-- λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- λ> flatten (List [])
-- []
data NestedList a = Elem a | List [NestedList a] deriving Show

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- 08 Eliminate consecutive duplicates of list elements

-- 09 Pack consecutive duplicates of list elements into sublists

-- 10 Run-length encoding of a list
