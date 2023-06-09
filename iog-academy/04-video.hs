{-
    - Patter matching in functions
        - Catch-all patterns
    - Closer look at lists
    - Pattern matching
        - List
        - Tuples
    - Case expressions
    - Declaration style vs Expression style
-}

-- Pattern matching is the act of matching data against a pattern, optionally
-- binding variables to successful matches

specialBirthday' :: Int -> String
specialBirthday' age =
    if age == 1
        then "First birthday"
        else
            if age == 18
                then "You are an adult"
                else
                    if age == 60
                        then "Finally, I can stop caring about new lingo!"
                        else "Nothing special"


-- 1. You pattern matching have to exhaustive and match all possible scenarios
-- 2. Always pay attention to the order that you are patter matching. Make sure
-- the catch-all pattern is the last one (any after it will be unreachable)
specialBirthday :: Int -> String
specialBirthday 1 =
    "First birthday!"
specialBirthday 18 =
    "You are an adult"
specialBirthday 60 =
    "Finally, I can stop caring about new lingo!"
specialBirthday _ =
    "Nothing special"


-- In haskell [1, 2, 3] is syntatic sugar for 1:2:3:[] that's is important when
-- pattern matching
inList :: [Int] -> String
inList [] =
    "It's empty!"
inList [x] =
    "A single elem: " ++ show x
inList [x, y] =
    "Two elems: " ++ show x ++ " and " ++ show y
inList (x:y:z:[]) =
    "Three elems: " ++ show x ++ ", " ++ show y ++ " and " ++ show z
inList (x:rest) =
    "The first elem is: " ++ show x ++ " and the rest is: " ++ show rest


-- If the pattern does not match. You ignore the parameters with '_' and go to
-- catch-all case
firstAndThird :: [Bool] -> String
firstAndThird (x:_:z:_) =
    "The first and third elements are: " ++ show x ++ " and " ++ show z
firstAndThird _ =
    "Don't have them"


initials :: String -> String -> String
initials name last =
    if name == "" || last == ""
        then "Please inform your name and last name"
        else
            let
                x = head name
                y = head last
            in
                [x] ++ "." ++ [y] ++ "."


-- It will match any two string that have at least the first letter. So empty
-- strings won't match and will be on catch-all case
initials' :: String -> String -> String
initials' (f:_) (l:_) =
    [f] ++ "." ++ [l] ++ "."
initials' _ _ =
    "Please inform your name and last name"


-- Pattern matching with tuples
firstOfThree :: (a, b, c) -> a
firstOfThree (x, _, _) = x


checkForZeros :: (Int, Int, Int) -> String
checkForZeros tuple =
    let
        msg = case tuple of
            (0, _, _) -> "The first is a zero"
            (_, 0, _) -> "The second is a zero"
            (_, _, 0) -> "The three is a zero"
            _         -> "There is no zeros"
    in
        "The " ++ show tuple ++ " has " ++ show msg


checkForZeros' :: (Int, Int, Int) -> String
checkForZeros' tuple =
        "The " ++ show tuple ++ " has " ++ show msg
    where
        msg = case tuple of
            (0, _, _) -> "The first is a zero"
            (_, 0, _) -> "The second is a zero"
            (_, _, 0) -> "The three is a zero"
            _         -> "There is no zeros"


{-
    Declaration vs Expression Type

        The declaration style is where you formulate an algorithm in terms of
    several equaltions to be satisfied

        The expression style is where you compose big expressions from small
    expressions

    Declaration
        1. Where
        2. Pattern matching
        3. Guards
        4. Functions: fn x = x + x

    Expression
        1. Let
        2. Case expressions
        3. If expressions
        4. Lambda abstractions: fn \x -> x + x
-}
