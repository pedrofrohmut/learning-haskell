{-
    - Highorder functions
        - filter
        - any
    - Lambda functions
    - Precedence and associativity
    - Curried functions
        - Partial applications
    - Applying and composing functions
        - The $ operator
        - The . operator
-}

-- Highorder functions are functions the receives another function as argument
-- or returns a function as a result

complexFunc1 :: Int -> Int
complexFunc1 x = x + 1

func1 :: Int -> Int
func1 x =
    complexFunc1 (complexFunc1 x)

complexFunc2 :: Int -> Int
complexFunc2 x = x + 2

func2 :: Int -> Int
func2 x =
    (complexFunc2 (complexFunc2 x)) + (complexFunc2 (complexFunc2 x))

-- Apply the function to x twice
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

func1' :: Int -> Int
func1' x =
    applyTwice complexFunc1 x

func2' :: Int -> Int
func2' x =
    applyTwice complexFunc2 x + applyTwice complexFunc2 x

-- The filter function
-- filter :: (a -> Bool) -> [a] -> [a]
evens =
    filter even [1..20]

fruitsWithAnA =
    filter tempFunct ["Apple", "Banana", "Pear", "Grape", "Wood"]
    where
        tempFunct x = 'a' `elem` x


-- The any function
-- any :: Foldable t => (a -> Bool) -> t a -> Bool

biggerThan4 :: Int -> Bool
biggerThan4 x = x > 4

anyBigger = any biggerThan4 [1, 2, 3, 4, 5]

cars = [("Toyota", 0), ("Nissan", 3), ("Ford", 1)]


hasCar (_, n) = n > 0

areThereCars = any hasCar cars


-- Lambda functions: also called anonymus functions, is a function definition that
-- doesn't have a name

anyBigger1 = any (\x -> x > 4) [1, 2, 3, 4, 5, 6]

anyBigger2 = any (\x -> x > 4) [1, 2, 3]

-- '>' operator is a function so you can partially apply it
anyBigger3 = any (> 4) [1, 2, 3, 4]

areThereCars1 = any (\(_, n) -> n > 0) cars

fruitsWithAnA1 =
    filter (\x -> 'a' `elem` x) ["Apple", "Banana", "Pear", "Grape", "Wood"]

-- Currying: In Haskell every function is curried
add3 :: Int -> Int -> Int -> Int
add3 a b c = a + b + c

-- With parenthesis
add3' :: Int -> (Int -> (Int -> Int))
((add3' a) b) c = a + b + c

-- With lambdas
(add3'' a) b = \c -> a + b + c

add3''' a = \b -> (\c -> a + b + c)

add3'''' = \a -> (\b -> (\c -> a + b + c))

{-
    How it works

    add3 = \a -> \b -> \c -> a + b + c

    when you call: add3 13 42 69

    1. add3 13 = \b -> \c -> 13 + b + c  :: Int -> (Int -> Int)
    2. add3 13 42 = \c -> 13 + 42 + c    :: Int -> Int
    3. add3 13 42 69 = 13 + 42 + 69      :: Int
-}
add3Values = \a -> \b -> \c -> a + b + c

-- Partial Application
createEmail :: String -> String -> String -> String
createEmail domain name lastName =
    name ++ "." ++ lastName ++ "@" ++ domain

-- Here you partially apply the createEmail function only provinding the domain
createEmailTeckel :: String -> String -> String
createEmailTeckel =
    createEmail "teckel-owners.com"

-- Here you partially apply the createEmail function only provinding the domain
createEmailSCL :: String -> String -> String
createEmailSCL =
    createEmail "secret-cardano-lovers.com"

-- Using the partial function you can create an email only providing the remaining
-- two arguments: name and lastName
email1 = createEmailTeckel "Robertino" "Martinez"
email2 = createEmailSCL "Vitalik" "Buterin"

-- How to use partial application when you want to provide an argument that is not
-- the first from left? With a helper function
createEmailJohn domain lastName =
    createEmail domain "John" lastName

-- Currying: Is a technique that allows you to transform a function with multiple
-- arguments into a chain of functions with one single argument
addFunc :: Int -> Int -> Int
addFunc x y = x + y

-- Partial application of addFunc to create a new function addFive :: Int -> Int
addFive :: Int -> Int
addFive = addFunc 5

-- Same as \x -> x ++ "ing"
x1 = (++ "ing") "Think"

-- Same as \x -> "Anti" ++ x
x2 = ("Anti" ++) "library"

-- The function application $ operator
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x

{-
    The white space operator has the highest left-associative precedence
    The function application operator ($) has the lowest right associative
    precedence: infixr 0 $

    f g h x = ((f g) h) x

    f $ g $ h x = f (g (h x))

    ($)  :: (a -> b) -> a -> b

    Here we use the '$' operator to remove parenthesis:

    show ((2**) (max 3 (2 + 2)))

    show $ (2**) (max 3 (2 + 2))

    show $ (2**) $ max 3 (2 + 2)

    show $ (2**) $ max 3 $ 2 + 2

    *All these expressions are equivalent
-}

{-
    Function Composition

    (.) :: (b -> c) -> (a -> b) -> a -> c

    from: show ((2**) (max 3 (2 + 2)))

    to:   show . (2**) . max 3 $ 2 + 2
-}

-- Complicated function using parenthesis
complicatedFunc :: [Int] -> Bool
complicatedFunc list =
    any even (filter (> 25) (tail (take 10 list)))

-- Complicated function using the '.' operator for function composition
complicatedFunc' :: [Int] -> Bool
complicatedFunc' list =
    any even . filter (> 25) . tail . take 10 $ list

{-
    Point-free style
-}

fourOurLarger :: Int -> Int
fourOurLarger x = max 4 x

add1 :: Int -> Int
add1 x = 1 + x

-- Here the argument is omitted
fourOurLarger' :: Int -> Int
fourOurLarger' = max 4

-- The argument is implicit in the function body and explicit in the definition
add1' :: Int -> Int
add1' = (+ 1)

-- Complicated function using the '.' operator for function composition
-- And omitting the argument
complicatedFunc'' :: [Int] -> Bool
complicatedFunc'' =
    any even . filter (> 25) . tail . take 10
