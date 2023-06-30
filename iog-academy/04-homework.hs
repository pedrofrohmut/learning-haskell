-- Question 1
-- Lets say you have the nested values defined bellow. How would you get the value of
-- 4 by using only pattern matching in a function?

nested :: [([Int], [Int])]
nested = [([1,2],[3,4]), ([5,6],[7,8])]

fn1 :: [([Int], [Int])] -> Int
fn1 [(_, [_, x]), _] = x

-- Question 2
-- Write a function that takes a list of elements of any type and, if the list
-- has 3 or more elements, it removes them. Else, it does nothing. Do it two
-- times, one with multiple function definitions and one with case expressions.

fn2 :: [a] -> [a]
fn2 (_:_:_:x:rest) = x : rest
fn2 list = list

fn2' :: [a] -> [a]
fn2' list = case list of
    (_:_:_:x:rest) -> x : rest
    list -> list

-- Question 3
-- Create a function that takes a 3-element tuple (all of type Integer) and adds them together

fn3 :: (Float, Float, Float) -> Float
fn3 (a, b, c) = a + b + c

-- Question 4
-- Implement a function that returns True if a list is empty and False otherwise.

isEmptyList :: [a] -> Bool
isEmptyList [] = True
isEmptyList _  = False

-- Question 5
-- Write the implementation of the tail function using pattern matching. But,
-- instead of failing if the list is empty, return an empty list.

listTail :: [a] -> [a]
listTail list =
    case list of
        []        -> []
        [_]       -> []
        (_:tail)  -> tail

listTail' :: [a] -> [a]
listTail' list
    | null list = []
    | otherwise = tail
    where (x:tail) = list

-- Question 6
-- write a case expression wrapped in a function that takes an Int and adds one
-- if it's even. Otherwise does nothing.
-- (Use the `even` function to check if the number is even.)

fn6 :: Int -> Int
fn6 x =
    if mod x 2 == 0
        then x + 1
        else x

fn6' :: Int -> Int
fn6' x =
    case even x of
        True  -> x + 1
        False -> x

--------------------------------------------------------------------------------

assert exp res name =
    if exp == res
        then name ++ " passed."
        else "Failed test for: " ++ name ++ "."

main :: IO ()
main = do
    -- fn2 it returns elem 4 to n if list len bigger than 3 of the list otherwise
    putStrLn $ assert (fn2 []) ([]::[Int]) "Test 2.1"
    putStrLn $ assert (fn2 [1,2]) [1,2] "Test 2.2"
    putStrLn $ assert (fn2 [1,2,3]) [1,2,3] "Test 2.3"
    putStrLn $ assert (fn2 [1,2,3,4]) [4] "Test 2.4"

    -- fn3 takes a 3 int tuple and sum it
    putStrLn $ assert (fn3 (1,2,3)) 6 "Test 3.1"
    putStrLn $ assert (fn3 (4,6,3)) 13 "Test 3.2"
    putStrLn $ assert (fn3 (10,(-2),3)) 11 "Test 3.3"

    -- isEmptyList check if list is empty
    putStrLn $ assert (isEmptyList []) True "Test 4.1"
    putStrLn $ assert (isEmptyList [1,2,3]) False "Test 4.2"

    -- listTail gets the tail of the list or empty list if empty
    putStrLn $ assert (listTail []) ([]::[Int]) "Test 5.1"
    putStrLn $ assert (listTail [1]) ([]::[Int]) "Test 5.2"
    putStrLn $ assert (listTail [1,2,3]) [2,3] "Test 5.3"

    -- fn6 takes an elem and add one if its even or just return if not
    putStrLn $ assert (fn6 1) 1 "Test 6.1"
    putStrLn $ assert (fn6 2) 3 "Test 6.2"
