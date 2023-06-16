{-
    - Why recursion?
    - Think Recursively
        - sum and product
    - Steps to create your own recursive function
    - Examples of recursion
        - and, length, reverse, drop, take, map, filter
    - Extracing the foldr pattern
    - The foldl pattern
    - The foldl' function
    - When to use foldr, foldl, and foldl'
-}

{-------------------------------------------------------------------------------
    Recursion:

    1. Everything you can do with loops, you can do with recursion
    2. Many functions can naturally be defined using recursion
    3. Some functions are more clear and more concise if defined using recursion
    4. You can use induction to do mathematical reasoning and prove properties of
    functions defined using recursion
-}

{-
    Python code:

    def sum(list):
        total = 0
        for i in list:
            total = total + i
        return total
-}

-- Sum with recursion
-- Empty list: base case return 0
-- Not empty list: sum first element with the recursive call with the rest of the list
mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

mySum' :: [Int] -> Int
mySum' (x:[]) = x
mySum' (x:xs) = x + mySum xs


myProd :: [Int] -> Int
myProd [] = 1
myProd (head : rest) = head * myProd rest

myProd' :: [Int] -> Int
myProd' (head:[])   = head
myProd' (head:rest) = head * myProd rest

{-
    Tips for writing recursive functions:

    1. Write down the type: This will help you define de function later
    2. Enumerate the possible cases you could have based on its inputs
    3. Between all the previously instantiated cases, identify which are the
    simplest ones and define them
    4. Think about what you have available
    5. Define the rest of the cases
    6. Reflect on the function
-}

-- and' Function that returns True if all elements of the list are True
--
-- The base case returns True because if it returns False it will always returns
-- False for any list provided and True will be True with True and False with False
-- and bring the desired result
and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

-- In this definition you have the base case not of an empty list but of a list
-- with a single element
and'' :: [Bool] -> Bool
and'' (x:[]) = x
and'' (x:xs) = x && and' xs


-- length' Function that returns the length of a list
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

-- Here you need the base and emptyList case because the base case wont work for
-- empty lists by default
-- *It this case* opting for a base case of emptyList will result in a smaller implementation
length'' :: [a] -> Int
length'' []     = 0
length'' (x:[]) = 1
length'' (x:xs) = 1 + length'' xs


-- reverse' Function that reverse the position of the elements of a list
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' (x:[]) = [x]
reverse'' (x:xs) = reverse' xs ++ [x]


{-
    drop' Remove the first n elements of the list

    In this case with have 2 arguments and 2 base cases
    Case 1: There is a n but the list is empty => just return an emtpy list
    Case 2: The list is not empty but n is zero (nothing to drop) => return the list
    Case 3: Recursive call with n-1 and the rest of the list
-}
drop' :: Int -> [a] -> [a]
drop' 0 []     = []
drop' n []     = []
drop' 0 (x:xs) = x:xs
drop' n (x:xs) = drop' (n - 1) xs

-- 1. Drop '0 []' since any int with emptyList returns emptyList
-- 2. Ignore the int with the empty list, it is not used
-- 3. Ignore the x in recursive case, it is not used
-- 4. Remove pattern matching in '0 (x:xs)' case, it is not necessary
drop'' :: Int -> [a] -> [a]
drop'' _ []     = []
drop'' 0 xs     = xs
drop'' n (_:xs) = drop' (n - 1) xs

-- In the '0 xs' case replace 0 for n so you can use a guard and then add the
-- guard the catch n <= 0
-- Before the match for negative values this function goes into a infinit loop
drop''' :: Int -> [a] -> [a]
drop''' _ []          = []
drop''' n xs | n <= 0 = xs
drop''' n (_:xs)      = drop' (n - 1) xs


-- take' Function return the first n elements of a list as a subset of a list
take' :: Int -> [a] -> [a]
take' _ []          = []
take' n xs | n <= 0 = []
take' n (x:xs)      = x : take' (n-1) xs


-- take function using guards
take'' :: Int -> [a] -> [a]
take'' n list
    | n <= 0           = []
    | length list == 0 = []
    | otherwise        = x : take'' (n-1) xs
    where
        (x:xs) = list


-- map' High-order function that applies a function to every other element in the list
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs


-- filter' High-order function filters out the elements that do not satisfy a predicate
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' f (x:xs) =
    if f x
        then x : filter' f xs
        else     filter' f xs


-- The functionality as above but with guards instead of if/else
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' _ []     = []
filter'' f (x:xs)
    | f x       = x : filter' f xs
    | otherwise =     filter' f xs


-- Foldr Pattern ---------------------------------------------------------------
-- foldr means fold right to left

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' func value []     = value -- base case
foldr' func value (x:xs) = func x (foldr' func value xs) -- recursive case

{-
    so using 'foldr (+) 0 [1,2,3,4]' is the same of 'sum [1,2,3,4]'
    and 'foldr (*) 1 [1,2,3,4]' is the same of 'product [1,2,3,4]'
-}

-- Partially apply the foldr with the function and the base value
sumFoldr :: [Int] -> Int
sumFoldr = foldr (+) 0

productFoldr :: [Int] -> Int
productFoldr = foldr (*) 1

andFoldr :: [Bool] -> Bool
andFoldr = foldr (&&) True

{-
    -- length' Function that returns the length of a list
    length' :: [a] -> Int
    length' []     = 0
    length' (_:xs) = (+) 1 length' xs
-}
