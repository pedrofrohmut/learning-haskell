{-
    Creating Non-parameterized Types

    - Type Synonyms
        - How to define them
        - Why use them
    - New types with data
        - Creating types
        - Using types
        - Value Parameters
    - Record syntax
-}

{-
    Type synonyms

    Exp:
        type String = [Char]

    !! When you create a type synonym, you are not creating a new type. You're
    telling Haskell that an existing type can be referred with a differente name
    (a synonym) !!

    Why use Type Synonyms?

    generateTx :: String -> String -> Int -> String
    generateTx from to value = from ++ to ++ show value

    Here you use type synonym to make more clear the pourpose/intension of the function
-}

type Addr = String
type Value = Int
type Id = String

generateTx :: Addr -> Addr -> Value -> Id
generateTx from to value = from ++ to ++ show value

type Name = String
type Address = (String, Int)
type Person = (Name, Address)

bob =   ("Bob Smith", ("Main St.", 555)) :: Person
-- :t bob > bob::Person
-- fst bob > ::Name

{-
    Defining new types with data

    data PaymentMethod = Cash | Card | Cryptocurrency

    data Color = Red | Green | Blue

    data Boolean = True | False -- Real definition of Boolean

    data Ordering = LT | EQ | GT  -- Real difinition of Ordering

    !! The type name and the value constructors must start with an uppercase letter !!
-}

-- deriving Show makes this type an instance of the Show type
data PaymentMethod = Cash | Card | Cryptocurrency deriving (Show)

type Person2 = (Name, Address, PaymentMethod)

bob2 = ("Bob Smith", ("Main St.", 555), Cash) :: Person2

howItPays :: Person2 -> String
howItPays (_, _, Cash)           = "Pays with Cash"
howItPays (_, _, Card)           = "Pays with Card"
howItPays (_, _, Cryptocurrency) = "Pays with Cryptocurrency"

{-
    Value parameters

    data Shape = Circle Float | Rectangle Float Float

    :t Circle
    > Circle::Float -> Shape

    :t Rectangle
    > Rectangle::Float -> Float -> Shape
-}

data Shape = Circle Float | Rectangle Float Float deriving (Show)

smallCircle = Circle 3
hugeCircle = Circle 100

rect1 = Rectangle 10 5
rect2 = Rectangle 256 128

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle l1 l2) = l1 * l2

smallCircleArea = area smallCircle
rect2Area = area rect2

{-
    -- coords radius/(side side) color

    data Shape = Circle    (Float, Float) Float String
               | Rectangle (Float, Float) Float Float String

    In this example above it works but will be hard to work with
    lots of pattern matching and complicated syntax
-}

{-
    Record Syntax

    Without record syntax we have:

    data Employee = Employee String Float

    With record syntax:

    data Employee =
        Employee { name :: String , experienceInYears :: Int } deriving (Show)
-}
data Employee = Employee { name :: String , experienceInYears :: Float } deriving (Show)

-- :type richard > richard::Employee
richard = Employee { name = "Richard", experienceInYears = 7.5 }

-- If you pass the parameters in the same order that they are defined you can
-- reduce the boilerplate like bellow
matt = Employee "Matt" 5

-- You create a new Employee by passing another one and only specifying the
-- fields that change
updatedMatt = matt { experienceInYears = 6 }

{-
    When using Record Syntax Haskell auto-generates the functions for name and
    experienceInYears

    name :: Employee -> String
    experienceInYears :: Employee -> Float

    name matt -- "Matt"
    experienceInYears matt -- 5.0
-}

team = [Employee "John" 4, Employee "Josh" 2, Employee "Matt" 7]

combinedExp :: [Employee] -> Float
combinedExp = foldr (\e acc -> experienceInYears e + acc) 0

teamExp = combinedExp team

{-

    Without Record Syntax:

    data Shape = Circle    (Float, Float) Float String
               | Rectangle (Float, Float) Float Float String
               deriving    (Show)

    With Record Syntax:

    data Shape
        = Circle
            { position :: (Float, Float)
            , radius   :: Float
            , color    :: String
            }
        | Rectangle
            { position :: (Float, Float)
            , width    :: Float
            , height   :: Float
            , color    :: String
            }
        deriving (Show)
-}
data Shape'
    = Circle
        { position :: (Float, Float)
        , radius   :: Float
        , color    :: String
        }
    | Rectangle
        { position :: (Float, Float)
        , width    :: Float
        , height   :: Float
        , color    :: String
        }
    deriving (Show)

circ = Circle { position = (1, 2) , radius = 6.0 , color = "Green" }

rect3 = Rectangle { position = (2, 4)
                  , width    = 12
                  , height   = 8
                  , color    = "Red"
                  }

-- Use the reduced version where you must respect the order that the parameters
-- are defined in the data type
rect4 = Rectangle (9,3) 7 3 "Yellow"

rect5 = rect3 { width = 12 }

{-
    Pattern matching
-}

-- pattern matching without using the Record Syntax
area1 :: Shape -> Float
area1 (Circle _ r _) = pi * r^2
area1 (Rectangle _ w h _) = w * h

{-
    The pattern matching is equivalent to the above example with a different syntax

    !! if you change the declaration of the Circle and Rectangle by adding removing
    and renaming fields but you don't touch on any of the field matched on these
    functions the matching will still work and you don't have to refactor those !!
-}
area2 :: Shape -> Float
area2 Circle {radius=r} = pi * r^2
area2 Rectangle {height=h, width=w} = h * w
