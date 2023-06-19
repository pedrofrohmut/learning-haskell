-- Question 1
-- Write a function called `repeat'` that takes a value and creates an infinite
-- list with the value provided as every element of the list.
-- >>> repeat 17
--[17,17,17,17,17,17,17,17,17...

repeat' :: Enum a => a -> [a]
repeat' x = [x..]

q1_1 = take 10 (repeat 17)

-- Question 2
-- Using the `repeat'` function and the `take` function we defined in the lesson
-- (comes with Haskell), create a function called `replicate'` that takes a number
-- `n` and a value `x` and creates a list of length `n` with `x` as the value of
-- every element. (`n` has to be Integer.)
--
-- >>> replicate 0 True
-- []
-- >>> replicate (-1) True
-- []
-- >>> replicate 4 True
-- [True,True,True,True]

replicate' :: a -> Int -> [a]
replicate' x n = take n $ repeat x

-- 1 list with length of 69 and value == 42
q2_1 = replicate' 42 69

-- Question 3
-- Write a function called `concat'` that concatenates a list of lists.
--
-- >>> concat' [[1,2],[3],[4,5,6]]
-- [1,2,3,4,5,6]

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

q3_1 = concat' [[1,2],[3],[4,5,6]]

-- Question 4
-- Write a function called `zip'` that takes two lists and returns a list of
-- corresponding pairs (zips them) like this:
--
-- >>> zip' [1, 2] ['a', 'b']
-- [(1,'a'),(2,'b')]
--
-- If one input list is shorter than the other, excess elements of the longer
-- list are discarded, even if one of the lists is infinite:
--
-- >>> zip' [1] ['a', 'b']
-- [(1,'a')]
-- >>> zip' [1, 2] ['a']
-- [(1,'a')]
-- >>> zip' [] [1..]
-- []
-- >>> zip' [1..] []
-- []

-- Zip example with function args pattern matching
zip' :: [a] -> [b] -> [(a, b)]
zip' _  []         = []
zip' [] _          = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- Zip example with guards
zip'' :: [a] -> [b] -> [(a, b)]
zip'' listA listB
    | null listA = []
    | null listB = []
    | otherwise  = (x,y) : zip' xs ys
    where
        (x:xs) = listA
        (y:ys) = listB

q4_1 = zip' [1] ['a', 'b']
-- [(1,'a')]
q4_2 = zip' [1, 2] ['a']
-- [(1,'a')]
q4_3 = zip' [] [1..]
-- []
q4_4 = zip' [1..] []
-- []

-- Question 5
-- Create a function called `zipWith'` that generalises `zip'` by zipping with a
-- function given as the first argument, instead of a tupling function.
--
-- > zipWith' (,) xs ys == zip' xs ys
-- > zipWith' f [x1,x2,x3..] [y1,y2,y3..] == [f x1 y1, f x2 y2, f x3 y3..]
--
-- For example, `zipWith' (+)` is applied to two lists to produce the list of
-- corresponding sums:
--
-- >>> zipWith (+) [1, 2, 3] [4, 5, 6]
-- [5,7,9]

zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' func listA listB
    | null listA = []
    | null listB = []
    | otherwise  = (func x y) : zipWith' func xs ys
    where
        (x:xs) = listA
        (y:ys) = listB

q5_1 = zipWith' (+) [1, 2, 3] [4, 5, 6]

-- Question 6
-- Write a function called `takeWhile'` that takes a precate and a list and
-- returns the list up until an element that doesn't satisfy the predicate.
--
-- >>> takeWhile (< 3) [1,2,3,4,1,2,3,4]
-- [1,2]
-- >>> takeWhile (< 9) [1,2,3]
-- [1,2,3]
-- >>> takeWhile (< 0) [1,2,3]
-- []

takeWhile' predicate list
    | predicate x = x : takeWhile' predicate xs
    | otherwise   = []
    where
        (x:xs) = list

q6_1 = takeWhile (< 3) [1,2,3,4,1,2,3,4] -- [1,2]
q6_2 = takeWhile (< 9) [1,2,3]           -- [1,2,3]
q6_3 = takeWhile (< 0) [1,2,3]           -- []

-- Question 7 (More difficult)
-- Write a function that takes in an integer n, calculates the factorial n! and
-- returns a string in the form of 1*2* ... *n = n! where n! is the actual result.

{-
factorial :: Int -> Int
factorial n
    | n <= 1    = 1
    | otherwise = n * factorial (n - 1)

printFact :: Int -> String
printFact n
    | n <= 1 = "1"
    | otherwise = printFact (n - 1) ++ " * " ++ show n

printFactResult :: Int -> String
printFactResult n
    | n < 1 = "1"
    | otherwise = (printFact n) ++ " = " ++ (show $ factorial n)
-}

-- Combined version using where (where functions are private to printFactorial)
printFactorial :: Int -> String
printFactorial n
    | n < 0     = "does not support negative numbers"
    | n == 0    = "0! = 1"
    | otherwise = (printFact n) ++ " = " ++ (show $ factorial n)
    where
        factorial :: Int -> Int
        factorial n
            | n <= 1    = 1
            | otherwise = n * factorial (n - 1)

        printFact :: Int -> String
        printFact n
            | n <= 1 = "1"
            | otherwise = printFact (n - 1) ++ " * " ++ show n

q7_1 = printFactorial 1
q7_2 = printFactorial 5
q7_3 = printFactorial 10

-- Question 8
-- Below you have defined some beer prices in bevogBeerPrices and your order list
-- in orderList + the deliveryCost. Write a function that takes in an order and
-- calculates the cost including delivery. Assume that the two lists have the
-- beers in the same order.

bevogBeerPrices :: [(String, Double)]
bevogBeerPrices =
  [ ("Tak", 6.00),
    ("Kramah", 7.00),
    ("Ond", 8.50),
    ("Baja", 7.50)
  ]

orderList :: [(String, Double)]
orderList =
  [ ("Tak", 5),
    ("Kramah", 4),
    ("Ond", 7)
  ]

deliveryCost :: Double
deliveryCost = 8.50
