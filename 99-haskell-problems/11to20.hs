{-
    11. Modified run-length encoding

    Modify the result of problem 10 in such a way that if an element has no
    duplicates it is simply copied into the result list. Only elements with
    duplicates are transferred as (N E) lists.

    * (encode-modified '(a a a a b c c a a d e e e e))
    ((4 A) B (2 C) (2 A) D (4 E))

    ghci> encodeModified "aaaabccaadeeee"
    [Multiple 4 'a',Single 'b',Multiple 2 'c',
    Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

data Amt a = Multiple a | Single deriving Show

myModifiedEncoding :: Eq a => [a] -> [(Amt Int, a)]
myModifiedEncoding (x:xs) =
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
