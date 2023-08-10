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
    let
        count [] n = n
        count (y:ys) n = count ys (n + 1)
    in
        count list 0

-- 05 Reverse a list
myReverse :: [a] -> [a]
myReverse list =
    let
        reverseIt result [] = result
        reverseIt result (x:xs) = reverseIt (x : result) xs
    in
        reverseIt [] list

-- 06 Find out whether a list is a palindrome
isPolindrome :: Eq a => [a] -> Bool
isPolindrome list = list == (reverse list)

-- 07 Flatten a nested list structure
-- $> myFlatten (Elem 5)
-- [5]
-- $> myFlatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- $> myFlatten (List [])
-- []
data NestedList a = Elem a | List [NestedList a] deriving Show

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List []) = []
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)

-- 08 Eliminate consecutive duplicates of list elements
-- $> myComp "111222344456667"
-- "1234567"
myComp :: Eq a => [a] -> [a]
myComp (x:xs) =
    let
        comp res curr (y:ys)
            | null ys = if y == curr then res else res ++ [y]
            | y == curr = comp res curr ys
            | otherwise = comp (res ++ [y]) y ys
    in
        comp [x] x xs

-- 09 Pack consecutive duplicates of list elements into sublists
-- * (pack '(a a a a b c c a a d e e e e))
-- ((A A A A) (B) (C C) (A A) (D) (E E E E))

t09_1 = [1,1,1,1,2,3,3,1,1,4,5,5,5]
t09_2 = ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']

myPack :: Eq a => [a] -> [[a]]
myPack (x:xs) =
    let
        pack :: Eq a => [a] -> a -> [a] -> [[a]]
        pack tmp curr (y:ys)
            | null ys && y == curr = [y:tmp]
            | null ys && y /= curr = [tmp, [y]]
            | y == curr            = pack (y:tmp) curr ys
            | y /= curr            = tmp : pack [y] y ys
    in
        pack [x] x xs

-- 10 Run-length encoding of a list
