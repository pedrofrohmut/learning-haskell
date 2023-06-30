-- Question 1
-- Write a multiline comment below.

{-
    Multiline baby
-}


-- Question 2
-- Define a function that takes a value and multiplies it by 3.

tripleMe :: Float -> Float
tripleMe x = x * 3


-- Question 3
-- Define a function that calculates the area of a circle.

areaOfCircle :: Float -> Float
areaOfCircle radius = pi * (radius ^ 2)


-- Question 4
-- Define a function that calculates the volume of a cylinder by composing the
-- previous function together with the height of the cylinder.

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder radius height = (areaOfCircle radius) * height


-- Question 5
-- Define a function that takes the height and radius of a cylinder and checks
-- if the volume is greater than or equal to 42.

isCylinderBig :: Float -> Float -> Bool
isCylinderBig radius height = (volumeOfCylinder radius height) >= 42

assert exp res name =
    if exp == res
        then name ++ " passed."
        else "Failed test for: " ++ name ++ "."

main :: IO ()
main = do
    -- tripleMe x triple the result of x
    putStrLn $ assert (tripleMe 3) 9 "Test 1.1"
    putStrLn $ assert (tripleMe 5.25) 15.75 "Test 1.2"
    putStrLn $ assert (tripleMe (-2)) (-6) "Test 1.3"

    let radius = 3.0
        height = 10.0

    -- areaOfCircle get the area of a circle
    putStrLn $ assert (areaOfCircle radius) 28.274334  "Test 2.1"

    -- volumeOfCylinder
    putStrLn $ assert (volumeOfCylinder radius height) 282.74334  "Test 3.1"

    -- isCylinderBig checks if the cylinder size is bigger or equal 42
    putStrLn $ assert (isCylinderBig radius height) True "Test 4.1"


