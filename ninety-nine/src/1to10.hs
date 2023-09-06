{-
    Problem 1

    Find the last element of a list.

    (Note that the Lisp transcription of this problem is incorrect.)

    Example in Haskell:

    λ> myLast [1,2,3,4]
    4
    λ> myLast ['x','y','z']
    'z'
-}
myLast :: [a] -> a
myLast (x:xs)
    | null xs = x
    | otherwise = myLast xs

myLast' :: [a] -> a
myLast' (x:[]) = x
myLast' (x:xs) = myLast' xs

test01 :: IO ()
test01 = do
    let arr = [1,2,3,4,5]
    if (myLast arr) /= 5
        then error "Test 01: myLast not working"
        else putStrLn "Test 01: myLast works"
    if (myLast' arr) /= 5
        then error "Test 01: myLast' not working"
        else putStrLn "Test 01: myLast' works"

{-
    Problem 2

    Find the last-but-one (or second-last) element of a list.

    (Note that the Lisp transcription of this problem is incorrect.)

    Example in Haskell:

    λ> myButLast [1,2,3,4]
    3
    λ> myButLast ['a'..'z']
    'y'
-}
myButLast :: [a] -> a
myButLast (x1:x2:xs)
    | null xs = x1
    | otherwise = myButLast (x2:xs)

myButLast' :: [a] -> a
myButLast' (x:_:[]) = x
myButLast' (_:x:xs) = myButLast' (x:xs)

test02 :: IO ()
test02 = do
    let arr = [1,2,3,4,5]
    if (myButLast arr) /= 4
        then error "Test 02: myButLast not working"
        else putStrLn "Test 02: myButLast is working"
    if (myButLast' arr) /= 4
        then error "Test 02: myButLast' not working"
        else putStrLn "Test 02: myButLast' is working"

{-
    Problem 3

    Find the K'th element of a list.

    The first element in the list is number 1. Example:

    * (element-at '(a b c d e) 3)
    c

    Example in Haskell:

    λ> elementAt [1,2,3] 2
    2
    λ> elementAt "haskell" 5
    'e'
-}
elemAt :: [a] -> Int -> a
elemAt (x:xs) n
    | n == 0 = x
    | otherwise = elemAt xs (n - 1)

elemAt' :: [a] -> Int -> a
elemAt' (x:xs) 0 = x
elemAt' (x:xs) n = elemAt' xs (n - 1)

test03 :: IO ()
test03 = do
    let arr = [1,2,3,4,5]
    if (elemAt arr 2) /= 3
        then error "Test 03: elemAt not working"
        else putStrLn "Test 03: elemAt working"
    let arr = [1,2,3,4,5]
    if (elemAt' arr 2) /= 3
        then error "Test 03: elemAt' not working"
        else putStrLn "Test 03: elemAt' working"

{-
    Problem 4

    Find the number of elements in a list.

    Example in Haskell:

    λ> myLength [123, 456, 789]
    3
    λ> myLength "Hello, world!"
    13
-}
myLength :: [a] -> Int
myLength list =
    let
        count [] n = n
        count (y:ys) n = count ys (n + 1)
    in
        count list 0

{-
    Problem 5
    Reverse a list.

    Example in Haskell:

    λ> myReverse "A man, a plan, a canal, panama!"
    "!amanap ,lanac a ,nalp a ,nam A"
    λ> myReverse [1,2,3,4]
    [4,3,2,1]
-}
myReverse :: [a] -> [a]
myReverse list =
    let
        reverseIt result [] = result
        reverseIt result (x:xs) = reverseIt (x : result) xs
    in
        reverseIt [] list

{-
    Problem 6

    Find out whether a list is a palindrome.

    Hint: A palindrome can be read forward or backward; e.g. (x a m a x).

    Example in Haskell:

    λ> isPalindrome [1,2,3]
    False
    λ> isPalindrome "madamimadam"
    True
    λ> isPalindrome [1,2,4,8,16,8,4,2,1]
    True
-}
isPolindrome :: Eq a => [a] -> Bool
isPolindrome list = list == (reverse list)

{-
    Problem 7

    Flatten a nested list structure.

    Transform a list, possibly holding lists as elements into a `flat' list by
    replacing each list with its elements (recursively).

    Example:

    * (my-flatten '(a (b (c d) e)))
    (A B C D E)

    Example in Haskell:

    We have to define a new data type, because lists in Haskell are homogeneous.

     data NestedList a = Elem a | List [NestedList a]

    λ> flatten (Elem 5)
    [5]
    λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
    [1,2,3,4,5]
    λ> flatten (List [])
    []
-}
data NestedList a = Elem a | List [NestedList a] deriving Show

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List []) = []
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)

{-
    Problem 8

    Eliminate consecutive duplicates of list elements.

    If a list contains repeated elements they should be replaced with a single
    copy of the element. The order of the elements should not be changed.

    Example:

    * (compress '(a a a a b c c a a d e e e e))
    (A B C A D E)

    Example in Haskell:

    λ> compress "aaaabccaadeeee"
    "abcade"
-}
myComp :: Eq a => [a] -> [a]
myComp (x:xs) =
    let
        comp res curr (y:ys)
            | null ys = if y == curr then res else res ++ [y]
            | y == curr = comp res curr ys
            | otherwise = comp (res ++ [y]) y ys
    in
        comp [x] x xs

{-
    Problem 9

    Pack consecutive duplicates of list elements into sublists.

    If a list contains repeated elements they should be placed in separate sublists.

    Example:

    * (pack '(a a a a b c c a a d e e e e))
    ((A A A A) (B) (C C) (A A) (D) (E E E E))

    Example in Haskell:

    λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a',
                 'a', 'd', 'e', 'e', 'e', 'e']
    ["aaaa","b","cc","aa","d","eeee"]
-}

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

{-
    Problem 10
    Run-length encoding of a list.

    Use the result of Problem 9 to implement the so-called run-length encoding
    data compression method. Consecutive duplicates of elements are encoded as
    lists (N E) where N is the number of duplicates of the element E.

    Example:

    * (encode '(a a a a b c c a a d e e e e))
    ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

    Example in Haskell:

    λ> encode "aaaabccaadeeee"
    [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}
myEncode :: Eq a => [a] -> [(Int, a)]
myEncode (x:xs) =
    let
        incTuple :: (Int, a) -> (Int, a)
        incTuple (x, y) = (x + 1, y)

        newTuple :: a -> (Int, a)
        newTuple x = (1, x)

        encode :: Eq a => (Int, a) -> a -> [a] -> [(Int, a)]
        encode tmp curr (y:ys)
            | null ys && y == curr = [incTuple tmp]
            | null ys && y /= curr = [tmp, (1, y)]
            | y == curr            = encode (incTuple tmp) curr ys
            | y /= curr            = tmp : encode (newTuple y) y ys
    in
        encode (newTuple x) x xs

