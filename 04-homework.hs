-- Question 1
-- Lets say you have the nested values defined bellow. How would you get the value of
-- 4 by using only pattern matching in a function?

nested :: [([Int], [Int])]
nested = [([1,2],[3,4]), ([5,6],[7,8])]

fn1 :: [([Int], [Int])] -> Int
fn1 [(_, [_, x]), _] = x

-- Question 2
-- Write a function that takes a list of elements of any type and, if the list has 3 or more elements, it
-- removes them. Else, it does nothing. Do it two times, one with multiple function definitions and one with
-- case expressions.

fn2 :: [a] -> [a]
fn2 (_:_:_:rest) = rest
fn2 list = list


fn2' :: [a] -> [a]
fn2' list =
    case list of
        (_:_:_:rest) -> rest
        list         -> list


-- Question 3
-- Create a function that takes a 3-element tuple (all of type Integer) and adds them together

fn3 :: (Float, Float, Float) -> Float
fn3 (a, b, c) = a + b + c


-- Question 4
-- Implement a function that returns True if a list is empty and False otherwise.

isEmptyList [] = True
isEmptyList _  = False


-- Question 5
-- Write the implementation of the tail function using pattern matching. But,
-- instead of failing if the list is empty, return an empty list.

listTail list =
    case list of
        []        -> []
        [_]       -> []
        (_:tail)  -> tail


-- Question 6
-- write a case expression wrapped in a function that takes an Int and adds one if it's even.
-- Otherwise does nothing.
-- (Use the `even` function to check if the number is even.)

fn6 :: Int -> Int
fn6 x = if mod x 2 == 0 then x + 1 else x


fn7 :: Int -> Int
fn7 x =
    case even x of
        True  -> x + 1
        False -> x
