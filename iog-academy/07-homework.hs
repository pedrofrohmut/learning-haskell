-- Question 1
-- Investigate the `Bounded` type class. What behaviours it provides?

answer1 = "The Bounded class is used to name the upper and lower limits of a type. \
         \ like minInt with (minBound::Int) and maxInt with (maxBound::Int) \
         \ data Bounded = minBound a | maxBound a"

-- Question 2
-- The types Int and Word bellong to the same type classes. What is the difference
-- between them? Check maybe the maxBound and minBound parameter for both types.

answer2 = "The Word is for int singened numbers but Int can have negative numbers too"

-- Question 3
-- Investigate the `Enum` type class. What behaviours provides?

answer3 = "It define a type the can be ordered and enumerate like a sequence \
         \ You can then use succ (to get successor) and pred (to get predecessor) \
         \ in the sequence"

-- Question 4
-- Add the most general type signatures possible to the functions below.
-- Then uncomment the functions and try to compile.

f1 :: (Fractional a, Show a) => a -> a -> [Char] -> [Char]
f1 x y z = show (x / y) ++ z

f2 :: (Bounded a, Enum a, Eq a) => a -> a
f2 x = if x == maxBound then minBound else succ x

-- Question 5
-- Investigate the numeric type classes to figure out which behaviors they provide
-- to change between numeric types.

answer5 = "Is a web search..."
