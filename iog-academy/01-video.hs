-- Single line comment

{-
    Multiline comment
-}

-- checks if a value is greater than 18
greaterThan18 x = x > 18

myAge = 12
isGreaterThan18 = greaterThan18 myAge

-- A function to add 6 numbers
add6Numbers a b c d e f = a + b + c + d + e + f

-- A function that takes the temperature in Fahrenheiht a returns it in Celsius
fahrenheitToCelsius x = (x - 32) * 5 / 9

myTemp = fahrenheitToCelsius 212 -- should be 100

{-
    A infinite list is defined by [1..]

    Haskell is lazy and will only evaluate the first five elements of the list
    It will not evaluate elements if they are not needed.

    So it is okay to use infinit lists with Haskell.
-}
giveMe x = take x [1..]
myList = giveMe 5 -- [1, 2, 3, 4, 5]

cheapComputation = 7
expensiveComputation = sum [1..10000000]

{-
    The expensive computation will only be called if the first evaluation is
    not enough
-}
isDone = if cheapComputation > 5 || expensiveComputation > 5
            then "Done" else "Not done"

-- Triple a value
tripleIt :: Num a => a -> a
tripleIt x = x * 3

-- Area of a circle
circleArea radius = pi * radius ^ 2

-- A function that calculates the volume of a cylinder
-- pi is a constant representing the number pi
volumeOfACylinder radius height = circleArea radius * height

-- Checks if the volume of a cylinder is bigger than 42
isVolumeOfACylinderGreaterThan42 radius height = (volumeOfACylinder radius height) > 42
