-- TODO: 38 and 41 part 2

{-
    99 questions/31 to 41

    This is part of Ninety-Nine Haskell Problems, based on Ninety-Nine Prolog
    Problems.
-}

{-
    Problem 31

    (**) Determine whether a given integer number is prime.

    Example:

    * (is-prime 7)
    T

    Example in Haskell:

    λ> isPrime 7
    True
-}

isPrime :: Int -> Bool
isPrime n =
    let
        isPrime' :: Int -> Int -> Bool
        isPrime' num i
            | isDivisible = False
            | i > halfN = True
            | otherwise = isPrime' num (i + 1)
            where
                isDivisible = num `mod` i == 0
                halfN = num `div` i
    in
        isPrime' n 2

-- Primes between 1 and 100:
-- ghci> filter isPrime [0..100]

{-
   Problem 32

   (**) Determine the greatest common divisor of two positive integer numbers.

   Use Euclid's algorithm.

   Example:

   * (gcd 36 63)
   9

   Example in Haskell:

   λ> [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
   [9,3,3]
-}

myGCD :: Int -> Int -> Int
myGCD val1 val2 =
    let
        x = modulus val1
        y = modulus val2

        -- The same as the main func but the args are always positive
        myGCD' :: Int -> Int -> Int
        myGCD' a b
            | mod a b == 0 = b
            | mod b a == 0 = a
            | otherwise = helper a b 1 2

        helper :: Int -> Int -> Int -> Int -> Int
        helper a b res i
            | i > a || i > b = res
            | aIsDivisible && bIsDivisible = updateResAndGoNext
            | otherwise = justGoNext
            where
                aIsDivisible = mod a i == 0
                bIsDivisible = mod b i == 0
                updateResAndGoNext = helper a b i (i + 1)
                justGoNext = helper a b res (i + 1)
    in
        myGCD' x y
    where
        modulus :: Int -> Int
        modulus n = if n >= 0 then n else (n * (-1))

{-
   Problem 33

   (*) Determine whether two positive integer numbers are coprime.

   Two numbers are coprime if their greatest common divisor equals 1.

   Example:

   * (coprime 35 64)
   T

   Example in Haskell:

   λ> coprime 35 64
   True
-}

myIsComprime :: Int -> Int -> Bool
myIsComprime a b = (gcd a b) == 1

{-
   Problem 34

   (**) Calculate Euler's totient function phi(m).

   Euler's so-called totient function phi(m) is defined as the number of positive
   integers r (1 <= r < m) that are coprime to m.

   Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

   Example:

   * (totient-phi 10)
   4

   Example in Haskell:

   λ> totient 10
   4
-}

myTotientPhi :: Int -> (Int, [Int])
myTotientPhi m =
    let
        helper :: Int -> (Int, [Int]) -> Int -> (Int, [Int])
        helper m (phi, r) i
            | i == m = (phi, r)
            | isComprime = incResGoNext
            | otherwise = justGoNext
            where
                isComprime = (gcd m i) == 1
                incResGoNext = helper m (phi + 1, r ++ [i]) (i + 1)
                justGoNext = helper m (phi, r) (i + 1)

        initRes :: (Int, [Int])
        initRes = (1, [1])

        initIterator :: Int
        initIterator = 2
    in
        helper m initRes initIterator

{-
   Problem 35

   (**) Determine the prime factors of a given positive integer.

   Construct a flat list containing the prime factors in ascending order.

   Example:

   * (prime-factors 315)
   (3 3 5 7)

   Example in Haskell:

   λ> primeFactors 315
   [3, 3, 5, 7]
-}

myPrimeFactors :: Int -> [Int]
myPrimeFactors num =
    let
        helper n res i
            | i > n = res
            | nIsDivisible = updtNAndRes
            | otherwise = goNext
            where
                nIsDivisible = mod n i == 0
                updtNAndRes = helper (div n i) (res ++ [i]) i
                goNext = helper n res (i + 1)

        initRes :: [Int]
        initRes = []

        initIter :: Int
        initIter = 2
    in
        helper num initRes initIter


{-
   Problem 36

   (**) Determine the prime factors and their multiplicities of a given positive
   integer.

   Construct a list containing each prime factor and its multiplicity.

   Example:

   * (prime-factors-mult 315)
   ((3 2) (5 1) (7 1))

   Example in Haskell:

   λ> prime_factors_mult 315
   [(3,2),(5,1),(7,1)]
-}

myPrimeFactorsMult :: Int -> [(Int, Int)]
myPrimeFactorsMult num =
    let
        helper :: [(Int, Int)] -> Int -> (Int, Int) -> [Int] -> [(Int, Int)]
        helper _ curr _ [] = [(curr, 1)]
        helper res curr (val, amt) (y:ys)
            | null ys && curr == y = incTupleAddToRes
            | null ys && curr /= y = addTupleAndNewTupleToRes
            | curr == y = incTuple
            | otherwise = addResNewTuple
            where
                incTupleAddToRes = res ++ [(val, amt + 1)]
                addTupleAndNewTupleToRes = res ++ [(val, amt), (y, 1)]
                incTuple = helper res curr (val, amt + 1) ys
                addResNewTuple = helper (res ++ [(val, amt)]) y (y, 1) ys

        initRes = [] :: [(Int, Int)]

        (x:xs) = myPrimeFactors num :: [Int]
    in
        helper initRes x (x, 1) xs

{-
   Problem 37

   (**) Calculate Euler's totient function phi(m) (improved).

   See Problem 34 for the definition of Euler's totient function. If the list of
   the prime factors of a number m is known in the form of problem 36 then the
   function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2)
   (p3 m3) ...) be the list of prime factors (and their multiplicities) of a
   given number m. Then phi(m) can be calculated with the following formula:

   p1 => Prime factor 1
   m1 => Multiplicity of prime factor 1

   phi(m) = (p1 - 1) * p1 ** (m1 - 1) *
            (p2 - 1) * p2 ** (m2 - 1) *
            (p3 - 1) * p3 ** (m3 - 1) * ...

   Note that a ** b stands for the b'th power of a.
-}

myTotientPhiImproved :: Floating a => Int -> a
myTotientPhiImproved num =
    let
        primeFactors :: [(Int, Int)]
        primeFactors = myPrimeFactorsMult num

        helper :: (Integral a, Floating b) => (a, a) -> b
        helper (p1, m1) =
            let
                p = fromIntegral p1
                m = fromIntegral m1
            in
                (p - 1) * (p ** (m - 1))

        iterator [] = []
        iterator (x:xs) = (helper x) : iterator xs

        resArr = iterator primeFactors
    in
        foldr (*) 1 resArr

{-
   Problem 38

   (*) Compare the two methods of calculating Euler's totient function. (no
   solution required)

   Use the solutions of Problems 34 and 37 to compare the algorithms. Take the
   number of reductions as a measure for efficiency. Try to calculate phi(10090)
   as an example.
-}


{-
   Problem 39

   (*) A list of prime numbers in a given range.

   Given a range of integers by its lower and upper limit, construct a list of
   all prime numbers in that range.

   Example in Haskell:

   λ> primesR 10 20
   [11,13,17,19]
-}

listPrimes :: Int -> Int -> [Int]
listPrimes start end =
    let
        iterator :: Int -> [Int]
        iterator i
            | i == end = []
            | isPrime i = i : iterator (i + 1)
            | otherwise = iterator (i + 1)
    in
        iterator start

{-
   Problem 40

   (**) Goldbach's conjecture.

   Goldbach's conjecture says that every positive even number greater than 2 is
   the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most
   famous facts in number theory that has not been proved to be correct in the
   general case. It has been numerically confirmed up to very large numbers
   (much larger than we can go with our Prolog system). Write a predicate to
   find the two prime numbers that sum up to a given even integer.

   Example:

   * (goldbach 28)
   (5 23)

   Example in Haskell:

   λ> goldbach 28
   (5, 23)
-}

goldbach :: Int -> (Int, Int)
goldbach num =
    let
        primes :: [Int]
        primes = listPrimes 2 num

        iterator :: [Int] -> [Int] -> (Int, Int)
        iterator (_:xs) [] = iterator xs primes
        iterator (x:xs) (y:ys)
            | (x + y) == num = (x, y)
            | otherwise = iterator (x:xs) ys
    in
        iterator primes primes

{-
   Problem 41

   (**) A list of even numbers and their Goldbach compositions in a given range.

   Given a range of integers by its lower and upper limit, print a list of all
   even numbers and their Goldbach composition.

   In most cases, if an even number is written as the sum of two prime numbers,
   one of them is very small. Very rarely, the primes are both bigger than say 50.
   Try to find out how many such cases there are in the range 2..3000.

   Example:

   * (goldbach-list 9 20)
   10 = 3 + 7
   12 = 5 + 7
   14 = 3 + 11
   16 = 3 + 13
   18 = 5 + 13
   20 = 3 + 17
   * (goldbach-list 1 2000 50)
   992 = 73 + 919
   1382 = 61 + 1321
   1856 = 67 + 1789
   1928 = 61 + 1867

   Example in Haskell:

   λ> goldbachList 9 20
   [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
   λ> goldbachList' 4 2000 50
   [(73,919),(61,1321),(67,1789),(61,1867)]
-}

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList start end = map goldbach (filter (\x -> mod x 2 == 0) [start..end])
