{-
    - The awesomeness of type classes
    - What are type classes
    - Common type classes
        - Eq, Ord
        - Num, Integral, Floating
        - Read, Show
    - The most general valid type
    - Multiple constrains
-}

{-

    The awesomeness of type classes

    Type classes are retricted polimorphic types

    The Eq type class is all about equality. The types that are instances of Eq
    type class can say if two values of its type are equal or not by using the
    '==' (equal) and '/=' (not equal) functions

    True == False
    True /= False

    (==) :: Eq a => a -> a -> Bool
    (/=) :: Eq a => a -> a -> Bool

    Right of => 'a' means it takes two polimorphic types
    Left of  => 'Eq a' means the polimorphic types are constrain to be of type
    class Eq

    The fat arrow symbol is the class constrain symbol. It indicates that a polimophic
    type is constrain to be an instance of a type class

    function :: constrain  => normal function signature
-}
sqr :: Int -> Int
sqr x = x * x

fst :: (a, b) -> a
fst (x, _) = x

func1 :: Eq a => a -> a -> a
func1 x y = if x == y then x else y

{-
    The Ord type class is all about ordering. The types that are instances of the
    Ord type class con order their values and say which values is the biggest

    (<), (>), (<=), (>=) :: Ord a => a -> a -> Bool
    max, min             :: Ord a => a -> a -> a
    compare              :: Ord a => a -> a -> Ordering

    4 > 9      -- False
    'a' >= 'b' -- False

    min 11 12 -- 11
    max 11 12 -- 12

    compare 4 9         -- LT (4 is lesser than 9)
    'f' `compare` 'e'   -- GT ('f' is greater than 'e')
    True `compare` True -- EQ (True is equal to True)
-}

{-
    The Num type class

    (+) :: Num a => a -> a -> a
    (-) :: Num a => a -> a -> a
    (*) :: Num a => a -> a -> a

    5 - 1     -- 4
    8.9 + 0.1 -- 9.0
    'a' - 'b' -- ERROR! Char is not an instance of Num!
-}

add1 :: Num a => a -> a
add1 x = x + 1

{-
    The Integral type class is a generic type that includes all Numeric types that
    are not floating point numbers

    takes two numbers devide the first by the second and return only the integer
    part of the division (equivalent to floor)

    div :: Integral a => a -> a -> a

    3 `div` 5 -- 0
    div 5 2   -- 2
-}

{-
    The Fractional type class is for the number that have the fractional part
    and it includes both Double and Float

    (/) :: Fractional a => a -> a -> a

    10 / 5 -- 2.0
    5 / 2  -- 2.5
    10 / 3 -- 3.333...335

    :type (10 / 5)
    (10 / 5) :: Fractional a => a
-}

{-
    The Show type class

    show :: Show a => a -> String

    show (3 :: Int) -- "3"
    show True       -- "True"
-}

{-
    The Read type class

    read :: Read a => String -> a

    read "3" / 2          -- 1.5
    read "True" || False  -- True
    read "[1,2,3]" :: Int -- [1,1,3]

    read "3" -- Does't know which numeric type. Exception
    read "Turue" :: Bool -- "Turue" is not a valid Bool value. Exception
-}

fToC :: Fractional a => a -> a
fToC x = (x - 32) * 5 / 9

{-
    Multiple constrains for the same type variable
-}

-- It needs to be instance of Eq because you are comparing it to 3
-- It needs to be a number so you can perform (+) operation
skip3 :: (Eq a, Num a) => a -> a
skip3 x = if x == 3 then x + 1 else x

{-
    Constrains for multiple type variables
-}
isXBigger :: (Eq a, Num b) => a -> a -> b
isXBigger x y = if x > y then 1 else 0

mistery1 :: (Ord a, Fractional b) => a -> a -> b -> b
mistery1 x y z = if x > y then z / 2 else z

mistery2 :: (Num a, Ord a, Fractional b) => a -> a -> b -> b
mistery2 x y z = if x + 1 > y then z / 2 else z
