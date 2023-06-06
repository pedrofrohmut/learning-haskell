{-
    - Pragmatic intro to types
    - Function Signatures
    - Playing with functions
        - Variables
        - Infix and Prefix functions
    - Common Data Types
    - Polymorphic values and type variables
-}


-- Common types: Int, Integer, Float, Double, Bool, Char, String,


-- 1st line Fuction Signature:
--     <nameOfFunction> :: <typeOfParameter> -> <typeOfReturn>
-- 2nd line Function definition:
--     the actual body of the function, its implementation
squareIt :: Float -> Float
squareIt x = x * x


-- It means it take 2 ints and returns an int
prodOfTwo :: Int -> Int -> Int
prodOfTwo a b = a * b


-- A function with no parameters (is usually called definition)
name :: String
name = "Bob"

{-
    THIS IS NOT VALID HASKELL
    x = 3
    3 + x
    x = 5  -- Error: Multiple declarations of 'x'
    3 + x
-}

-- Haskell variables are IMMUTABLE

-- Prefix form: the name comes before the arguments
myProd a b = a * b
result = myProd 2 3 -- prefix form

-- Infix form: the function come between the arguments
result1 = 1 + 2 -- '+' is actually a function

-- Functions defined with only symbols will be automatically set as Infix Functions

result2 = (+) 1 2 -- in the prefix form

result3 = 2 `myProd` 8 -- in the Infix form


-- Lists (all elements are from the same type)
myList :: [Char]
myList = ['a', 'b', 'c']


-- Strings represents a list of char
myString = "hello, world!"


-- Tuples can store elements of different types and have fixed size
myTuple :: (Char, Bool)
myTuple = ('a', True)


-- Polymorphic values are values that can have multiple types
first :: (a, b) -> a
first (x, y) = x

{-
    Examples of polymorphic values are:

    fst :: forall a b. (a, b) -> a
    fst (1, 2)

    snd :: forall a b. (a, b) -> b
    snd (1, 2)

    head :: forall a. [a] -> a
    head myList

    tail :: forall a. [a] -> [a]
    tail myList

-}


-- Lists

-- Get list element at position 2
elemAtPos2 = [ 1, 2, 3, 4, 5, 6, 7 ] !! 2 -- (!!) :: forall a. [a] -> Int -> a

-- makes a list that goes from 0 to 20
list0to20 = [0..20]

-- makes a list from 2 to 20 with step of 2
list2to20step2 = [2, 4..20]

-- lists all the multiples of 5 from 5 to 100
listMultiplesOf5 = [5, 10..100]

-- list all lowercase letters
listLowercaseAlphabet = ['a'..'z']

-- List 10 to 1 step -1
listDecreasingOrder = [10, 9..1]

-- Infinit lists (you can omit the right part to get infinit list)
first10 = take 10 [1..]

-- Infinit lists with a step
first10odds = take 10 [1,3..]

-- Prepend elements to a list (cons ':' is faster than concat '++' for long lists)
prependedList = 2 : [3, 4, 5] -- (:) :: forall a. a -> [a] -> [a]

-- Concat two lists together
concatLists = [1, 2, 3] ++ [4, 5, 6] -- (++) :: forall a. [a] -> [a] -> [a]


-- List functions
myLength = lenght [1, 2, 3, 4] -- gets the lenght

isEmpty = null [2] -- checks if it is empty

mySum = sum [1, 2, 3, 4, 5, 6, 7] -- sum the elements

isItInTheList = 5 `elem` [6, 4, 56, 6, 7, 23, 1, 9, 5] -- checks if a elem is in the list
