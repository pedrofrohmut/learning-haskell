-- Question 1
-- Write a function called `repeat'` that takes a value and creates an infinite
-- list with the value provided as every element of the list.

repeat' :: Enum a => a -> [a]
repeat' val = val : repeat' val

-- Question 2
-- Using the `repeat'` function and the `take` function we defined in the lesson
-- (comes with Haskell), create a function called `replicate'` that takes a number
-- `n` and a value `x` and creates a list of length `n` with `x` as the value of
-- every element. (`n` has to be Integer.)

replicate' :: Int -> a -> [a]
replicate' len val = take len $ repeat val

-- Question 3
-- Write a function called `concat'` that concatenates a list of lists.

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- Question 4
-- Write a function called `zip'` that takes two lists and returns a list of
-- corresponding pairs (zips them) like this:

-- If one input list is shorter than the other, excess elements of the longer
-- list are discarded, even if one of the lists is infinite:

-- Zip example with function args pattern matching
zip' :: [a] -> [b] -> [(a, b)]
zip' _      []     = []
zip' []     _      = []
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

-- Question 5
-- Create a function called `zipWith'` that generalises `zip'` by zipping with a
-- function given as the first argument, instead of a tupling function.
--
-- > zipWith' (,) xs ys == zip' xs ys
-- > zipWith' f [x1,x2,x3..] [y1,y2,y3..] == [f x1 y1, f x2 y2, f x3 y3..]
--
-- For example, `zipWith' (+)` is applied to two lists to produce the list of
-- corresponding sums:

zipWith' :: (a -> a -> b) -> [a] -> [a] -> [b]
zipWith' func listA listB
    | null listA || null listB = []
    | otherwise  = (func x y) : zipWith' func xs ys
    where
        (x:xs) = listA
        (y:ys) = listB

-- Question 6
-- Write a function called `takeWhile'` that takes a precate and a list and
-- returns the list up until an element that doesn't satisfy the predicate.

takeWhile' :: (Ord a, Num a) => (a -> Bool) -> [a] -> [a]
takeWhile' pred list
    | null list    = []
    | not (pred x) = []
    | (pred x)     = x : takeWhile' pred xs
    where
        (x:xs) = list

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

-- Question 8
-- Below you have defined some beer prices in bevogBeerPrices and your order list
-- in orderList + the deliveryCost. Write a function that takes in an order and
-- calculates the cost including delivery. Assume that the two lists have the
-- beers in the same order.

bevogBeerPrices :: [(String, Double)]
bevogBeerPrices =
    [ ("Tak", 6.00)
    , ("Kramah", 7.00)
    , ("Ond", 8.50)
    , ("Baja", 7.50)
    ]

orderList :: [(String, Double)]
orderList =
    [ ("Tak", 5)
    , ("Kramah", 4)
    , ("Ond", 7)
    ]

deliveryCost :: Double
deliveryCost = 8.50

calculatePriceList orderList prices
    | null orderList = []
    | otherwise = (name, (amt * price)) : calculatePriceList xs ys
    where
        (x:xs) = orderList
        (y:ys) = prices
        name   = fst x
        amt    = snd x
        price  = snd y

calculateTotal priceList delivery
    | null priceList = 0
    | otherwise = orderTotal
    where
        prices = map (\item -> snd item) priceList
        orderTotal = delivery + (sum prices)

--------------------------------------------------------------------------------

assert exp res msg =
    if exp == res
        then msg ++ " passed"
        else "Error on: " ++ msg

main :: IO ()
main = do
    -- repeat' x creates an infinit list of x
    putStrLn $ assert (take 5 $ repeat' 3) [3, 3, 3, 3, 3] "Test 1.1"

    -- replicate' x n creates a list of x with length of n
    putStrLn $ assert (replicate' 3 42) [42, 42, 42] "Test 2.1"
    putStrLn $ assert (replicate' 0 True) ([]::[Bool]) "Test 2.2"
    putStrLn $ assert (replicate' (-1) True) ([]::[Bool]) "Test 2.3"
    putStrLn $ assert (replicate' 4 True) [True, True, True, True] "Test 2.4"

    -- concat' joind two or more lists into one
    putStrLn $ assert (concat' [[1,2],[3],[4,5,6]]) [1,2,3,4,5,6] "Test 3.1"
    putStrLn $ assert (concat' [[1,2],[3]]) [1,2,3] "Test 3.2"
    putStrLn $ assert (concat' [[1,2],[5,6]]) [1,2,5,6] "Test 3.3"

    -- zip' xs ys joins the two lists into one list of tuples for correspondent
    -- indexes [(x1, y1), (x2, y2), ..., (xn, yn)]
    putStrLn $ assert (zip' [1, 2] ['a', 'b']) [(1,'a'),(2,'b')] "Test 4.1"
    putStrLn $ assert (zip' [1] ['a', 'b']) [(1,'a')] "Test 4.2"
    putStrLn $ assert (zip' [1, 2] ['a']) [(1,'a')] "Test 4.3"
    putStrLn $ assert (zip' [] [1..]) ([]::[(Int, Int)])  "Test 4.4"
    putStrLn $ assert (zip' [1..] []) ([]::[(Int, Int)])  "Test 4.5"

    -- zipWith' f xs ys zips two list applying a function to two correspondent
    -- values f xs ys => [f x1 y1, f x2 y2, ..., f xn yn]
    putStrLn $ assert (zipWith' (+) [1, 2, 3] [4, 5, 6]) [5, 7, 9] "Test 5.1"
    putStrLn $ assert (zipWith' (*) [1, 2, 3] [4, 5, 6]) [4, 10, 18] "Test 5.2"
    putStrLn $ assert (zipWith' (\x y -> (x, y)) [1, 2, 3] [4, 5, 6])
        [(1, 4), (2, 5), (3, 6)] "Test 5.3"

    -- takeWhile' predicate list takes the first elem of the list and adds to
    -- result list while a passed predicate returns true or the list ends
    putStrLn $ assert (takeWhile' (< 3) [1,2,3,4,1,2,3,4]) [1,2] "Test 6.1"
    putStrLn $ assert (takeWhile' (< 9) [1,2,3]) [1,2,3] "Test 6.2"
    putStrLn $ assert (takeWhile' (< 0) [1,2,3]) [] "Test 6.3"

    -- printFactorial x makes a string with the arguments and the final result
    putStrLn $ assert (printFactorial 1) "1 = 1" "Test 7.1"
    putStrLn $ assert (printFactorial 2) "1 * 2 = 2" "Test 7.2"
    putStrLn $ assert (printFactorial 3) "1 * 2 * 3 = 6" "Test 7.3"

    -- calculatePriceList and calculateTotal
    putStrLn $ assert (calculatePriceList orderList bevogBeerPrices)
        [("Tak", 30.0), ("Kramah", 28.0), ("Ond", 59.5)] "Test 8.1"
    putStrLn $ assert
        (calculateTotal (calculatePriceList orderList bevogBeerPrices) deliveryCost)
        126.0 "Test 8.2"
