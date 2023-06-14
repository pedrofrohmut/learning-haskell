-- Create a higher-order function that takes 3 parameters: A function and the two
-- parameters that that function takes, and flips the order of the parameters.
-- For example this: `(/) 6 2` returns `3`. But this: `flip' (/) 6 2` returns `0.3333333333`

myFlip :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
myFlip f a b = f b a


-- Create the `uncurry'` function that converts a curried function to a function
-- on pairs. So this: `(+) 1 2` that returns `3` can be written as
-- `uncurry' (+) (1,2)` (with the two different arguments inside a pair).

myUncurry :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
myUncurry f (a, b) = f a b


-- Create the `curry'` function that converts an uncurried function to a curried
-- function. So this: `fst (1,2)` that returns `1` can be written as
-- `curry' fst 1 2` (with the tuple converted into two different arguments).

myCurry :: ((a, b) -> t) -> a -> b -> t
myCurry f a b = f (a, b)


-- Use higher-order functions, partial application, and point-free style to
-- create a function that checks if a word has an uppercase letter.
-- Start with using just higher-order functions and build from there.

-- not sure what, just skipping this one


-- Create the `count` function that takes a team ("Red", "Blue", or "Green") and
-- returns the amount of votes the team has inside `votes`.

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

countVotes votes team =
    length $ filter (== team) votes
    --length $ filter (\x -> x == team) votes


-- Create a one-line function that filters `cars` by brand and then checks if
-- there are any left.

cars :: [(String,Int)]
cars = [("Toyota",0), ("Nissan",3), ("Ford",1)]

-- One liner
_anyCarOfBrand :: [(String, Int)] -> String -> Bool
_anyCarOfBrand cars brand =
    (snd $ filter (\x -> fst x == brand) cars !! 0) > 0

anyOf = _anyCarOfBrand cars


-- With let expression to easy readability
_anyCarOfBrandLet :: [(String, Int)] -> String -> Bool
_anyCarOfBrandLet cars brand =
    let
        sameBrand = filter (\x -> fst x == brand) cars
        first     = sameBrand !! 0
        amount    = snd first
    in
        amount > 0

anyLet = _anyCarOfBrandLet cars


-- Pipe Operator is not built-in
x |> f = f x

-- With Pipe Operator
anyCarOfBrandPipe :: [(String, Int)] -> String -> Bool
anyCarOfBrandPipe cars brand =
    cars
        |> filter sameBrand
        |> (!! 0)
        |> snd
        |> (> 0)
    where
        sameBrand =  (\x -> fst x == brand)


-- With the $ operator (function composition)
anyCarOfBrandComp :: [(String, Int)] -> String -> Bool
anyCarOfBrandComp cars brand =
     (> 0) $ snd $ (!! 0) $ filter (\x -> fst x == brand) cars


isCarFromBrand :: (String, Int) -> String -> Bool
isCarFromBrand car brand =
    fst car == brand

anyCarOfBrandDot cars brand =
     (> 0) . snd . (!! 0) $ filter (\x -> fst x == brand) cars

filterByBrand cars brand =
    filter (\x -> fst x == brand) cars

-- Partially apply (only need to provide the brand later)
filterCarsByBrand =
    filterByBrand cars
