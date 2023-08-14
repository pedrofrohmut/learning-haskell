import System.Random

{-
    Problem 21

    Insert an element at a given position into a list.

    Example:

    * (insert-at 'alfa '(a b c d) 2)
    (A ALFA B C D)

    Example in Haskell:

    λ> insertAt 'X' "abcd" 2
    "aXbcd"
-}

insertAt :: a -> [a] -> Int -> [a]
insertAt value list index =
    let
        helper :: a -> Int -> [a] -> Int -> [a]
        helper _ _ [] _ = []
        helper val idx (y:ys) i
            | i == idx  = val : y : helper val idx ys (i + 1)
            | otherwise = y : helper val idx ys (i + 1)
    in
        helper value index list 1

{-
    Problem 22

    Create a list containing all integers within a given range.

    Example:

    * (range 4 9)
    (4 5 6 7 8 9)

    Example in Haskell:

    λ> range 4 9
    [4,5,6,7,8,9]
-}

myRange :: Int -> Int -> [Int]
myRange start end =
    let
        helper :: Int -> Int -> Int -> [Int]
        helper first last i
            | i == last = [last]
            | otherwise = i : helper first last (i + 1)
    in
        helper start end start

{-
    Problem 23

    Extract a given number of randomly selected elements from a list.

    Example:

    * (rnd-select '(a b c d e f g h) 3)
    (E D A)

    Example in Haskell:

    λ> rnd_select "abcdefgh" 3 >>= putStrLn
    eda

    import System.Random

    genArr :: Float -> Int -> StdGen -> [Int]
    genArr _size 0   _gen = []
    genArr size  len gen  =
        let (rnd_value, newGen) = random gen :: (Float, StdGen)
            value = floor $ (rnd_value * size) + 1
        in  value : genArr size (len - 1) newGen
-}

myRndSelected :: [a] -> Int -> [a]
myRndSelected list amount =
    let
        getRndPos :: [a] -> StdGen -> (Int, StdGen)
        getRndPos list gen = randomR (0, (length list) - 1) gen

        removeAt :: [a] -> Int -> (a, [a])
        removeAt list pos =
            let
                helper (val, rest) (y:ys) n i
                    | i == n = (y, rest ++ ys)
                    | otherwise = helper (val, rest ++ [y]) ys n (i + 1)
            in
                helper ((head list), []) list pos 0

        helper res n ys i gen
            | n == 0 = res
            | otherwise =
                let (rndPos, newGen) = getRndPos ys gen in
                let (removed, rest) = removeAt ys rndPos in
                helper (res ++ [removed]) (n - 1) rest 1 newGen
    in
        helper [] amount list 1 (mkStdGen 125)

{-
    Problem 24

    Lotto: Draw N different random numbers from the set 1..M.

    Example:

    * (rnd-select 6 49)
    (23 1 17 33 21 37)

    Example in Haskell:

    λ> diff_select 6 49
    [23,1,17,33,21,37]
-}

{-
    Problem 25

    Generate a random permutation of the elements of a list.

    Example:

    * (rnd-permu '(a b c d e f))
    (B A D C E F)

    Example in Haskell:

    λ> rnd_permu "abcdef"
    "badcef"
-}

{-
    Problem 26

    (**) Generate combinations of K distinct objects chosen from the N elements
    of a list.

    In how many ways can a committee of 3 be chosen from a group of 12 people?
    We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the
    well-known binomial coefficients). For pure mathematicians, this result may
    be great. But we want to really generate all the possibilities in a list.

    Example:

    * (combinations 3 '(a b c d e f))
    ((A B C) (A B D) (A B E) ... )

    Example in Haskell:

    λ> combinations 3 "abcdef"
    ["abc","abd","abe",...]
-}

{-
    Problem 27
    Group the elements of a set into disjoint subsets.

    a) In how many ways can a group of 9 people work in 3 disjoint subgroups of
    2, 3 and 4 persons? Write a function that generates all the possibilities
    and returns them in a list.

    Example:

    * (group3 '(aldo beat carla david evi flip gary hugo ida))
    ( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
    ... )

    b) Generalize the above predicate in a way that we can specify a list of
    group sizes and the predicate will return a list of groups.

    Example:

    * (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
    ( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
    ... )

    Note that we do not want permutations of the group members; i.e.
    ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...). However, we
    make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and
    ((CARLA DAVID) (ALDO BEAT) ...).

    You may find more about this combinatorial problem in a good book on discrete
    mathematics under the term "multinomial coefficients".

    Example in Haskell:

    λ> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
    [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
    (altogether 1260 solutions)

    λ> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
    [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
    (altogether 756 solutions)
-}

{-
    Problem 28

    Sorting a list of lists according to length of sublists.

    a) We suppose that a list contains elements that are lists themselves. The
    objective is to sort the elements of this list according to their length.
    E.g. short lists first, longer lists later, or vice versa.

    Example:

    * (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
    ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))

    Example in Haskell:

    λ> lsort ["abc","de","fgh","de","ijkl","mn","o"]
    ["o","de","de","mn","abc","fgh","ijkl"]

    b) Again, we suppose that a list contains elements that are lists themselves.
    But this time the objective is to sort the elements of this list according to
    their length frequency; i.e., in the default, where sorting is done
    ascendingly, lists with rare lengths are placed first, others with a more
    frequent length come later.

    Example:

    * (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
    ((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))

    Example in Haskell:

    λ> lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
    ["ijkl","o","abc","fgh","de","de","mn"]
-}
