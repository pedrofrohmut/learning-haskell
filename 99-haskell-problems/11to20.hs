data Amt a = Multiple a | Single deriving Show

{-
    Problem 11

    Modified run-length encoding.

    Modify the result of problem 10 in such a way that if an element has no
    duplicates it is simply copied into the result list. Only elements with
    duplicates are transferred as (N E) lists.

    Example:

    * (encode-modified '(a a a a b c c a a d e e e e))
    ((4 A) B (2 C) (2 A) D (4 E))

    Example in Haskell:

    λ> encodeModified "aaaabccaadeeee"
    [Multiple 4 'a',Single 'b',Multiple 2 'c',
     Multiple 2 'a',Single 'd',Multiple 4 'e']

    Prob 10 example:
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

myEncodeModified :: Eq a => [a] -> [(Amt Int, a)]
myEncodeModified list =
    let
        modify :: (Int, a) -> (Amt Int, a)
        modify (1, x) = (Single, x)
        modify (n, x) = (Multiple n, x)

        encoded = myEncode list
    in
        map modify encoded

{-
    Problem 12

    Decode a run-length encoded list.

    Given a run-length code list generated as specified in problem 11. Construct
    its uncompressed version.

    Example in Haskell:

    λ> decodeModified
    [Multiple 4 'a',Single 'b',Multiple 2 'c',
     Multiple 2 'a',Single 'd',Multiple 4 'e']
    "aaaabccaadeeee"

-}

prob12_1 :: [(Amt Int, Char)]
prob12_1 = [ (Multiple 4, 'a') , (Single, 'b') , (Multiple 2, 'c') ,
             (Multiple 2, 'a') , (Single, 'd') , (Multiple 4, 'e') ]

myDecodeModified :: [(Amt Int, a)] -> [a]
myDecodeModified list =
    let
        decodeTup :: (Amt Int, a) -> [a]
        decodeTup (Single, x)     = x : []
        decodeTup (Multiple 2, x) = x : decodeTup (Single, x)
        decodeTup (Multiple n, x) = x : decodeTup (Multiple (n - 1), x)

        decode :: [(Amt Int, a)] -> [a]
        decode []     = []
        decode (y:ys) = (decodeTup y) ++ decode ys
    in
        decode list

{-
    Problem 13

    Run-length encoding of a list (direct solution).

    Implement the so-called run-length encoding data compression method directly.
    I.e. don't explicitly create the sublists containing the duplicates, as in
    problem 9, but only count them. As in problem P11, simplify the result list
    by replacing the singleton lists (1 X) by X.

    Example:

    * (encode-direct '(a a a a b c c a a d e e e e))
    ((4 A) B (2 C) (2 A) D (4 E))

    Example in Haskell:

    λ> encodeDirect "aaaabccaadeeee"
    [Multiple 4 'a',Single 'b',Multiple 2 'c',
     Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

myEncodeDirect :: Eq a => [a] -> [(Amt Int, a)]
myEncodeDirect (x:xs) =
    let
        newTup :: a -> (Amt Int, a)
        newTup x = (Single, x)

        incTup :: (Amt Int, a) -> (Amt Int, a)
        incTup (Single, x)     = (Multiple 2, x)
        incTup (Multiple n, x) = (Multiple (n + 1), x)

        encod :: Eq a => (Amt Int, a) -> a -> [a] -> [(Amt Int, a)]
        encod tup curr (y:ys)
            | null ys && y == curr = [(incTup tup)]
            | null ys && y /= curr = [tup, (newTup y)]
            | y == curr = encod (incTup tup) curr ys
            | y /= curr = tup : encod (newTup y) y ys
    in
        encod (newTup x) x xs

{-
    Problem 14

    Duplicate the elements of a list.

    Example:

    * (dupli '(a b c c d))
    (A A B B C C C C D D)

    Example in Haskell:

    λ> dupli [1, 2, 3]
    [1,1,2,2,3,3]
-}

myDuplicateElements :: [a] -> [a]
myDuplicateElements [] = []
myDuplicateElements (x:xs) = x : x : myDuplicateElements xs

{-
    Problem 15

    Replicate the elements of a list a given number of times.

    Example:

    * (repli '(a b c) 3)
    (A A A B B B C C C)

    Example in Haskell:

    λ> repli "abc" 3
    "aaabbbccc"
-}

myReplicate :: [a] -> Int -> [a]
myReplicate [] _ = []
myReplicate (x:xs) n =
    let
        sublist :: a -> Int -> [a]
        sublist val n
            | n == 0 = []
            | otherwise = val : sublist val (n - 1)
    in
        (sublist x n) ++ myReplicate xs n

{-
    Problem 16

    Drop every N'th element from a list.

    Example:

    * (drop '(a b c d e f g h i k) 3)
    (A B D E G H K)

    Example in Haskell:

    λ> dropEvery "abcdefghik" 3
    "abdeghk"
-}

myDropEvery :: [a] -> Int -> [a]
myDropEvery [] _ = []
myDropEvery list n =
    let
        drop :: Int -> Int -> [a] -> [a]
        drop _ _ [] = []
        drop i n (y:ys)
            | i `mod` n == 0 = drop (i + 1) n ys
            | otherwise = y : drop (i + 1) n ys
    in
        drop 1 n list

{-
    Problem 17

    Split a list into two parts; the length of the first part is given.

    Do not use any predefined predicates.

    Example:

    * (split '(a b c d e f g h i k) 3)
    ( (A B C) (D E F G H I K))

    Example in Haskell:

    λ> split "abcdefghik" 3
    ("abc", "defghik")
-}

mySplit :: [a] -> Int -> ([a], [a])
mySplit list amt =
    let
        helper :: ([a], [a]) -> [a] -> Int -> ([a], [a])
        helper (lft, rgt) (y:ys) n
            | n > 0     = helper (lft ++ [y], rgt) ys (n - 1)
            | otherwise = (lft, (y:ys))
    in
        helper ([], []) list amt

{-
    Problem 18

    Extract a slice from a list.

    Given two indices, i and k, the slice is the list containing the elements
    between the i'th and k'th element of the original list (both limits included).
    Start counting the elements with 1.

    Example:

    * (slice '(a b c d e f g h i k) 3 7)
    (C D E F G)

    Example in Haskell:

    λ> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
    "cdefg"
-}

{-
    Problem 19

    Rotate a list N places to the left.

    Hint: Use the predefined functions length and (++).

    Examples:

    * (rotate '(a b c d e f g h) 3)
    (D E F G H A B C)

    * (rotate '(a b c d e f g h) -2)
    (G H A B C D E F)

    Examples in Haskell:

    λ> rotate ['a','b','c','d','e','f','g','h'] 3
    "defghabc"

    λ> rotate ['a','b','c','d','e','f','g','h'] (-2)
    "ghabcdef"
-}

{-
    Problem 20

    Remove the K'th element from a list.

    Example in Prolog:

    ?- remove_at(X,[a,b,c,d],2,R).
    X = b
    R = [a,c,d]

    Example in Lisp:

    * (remove-at '(a b c d) 2)
    (A C D)

    (Note that this only returns the residue list, while the Prolog version also
    returns the deleted element.)

    Example in Haskell:

    λ> removeAt 2 "abcd"
    ('b',"acd")
-}
