-- Conditionals and Helper Contructions

-- If/Else statement
checkLocalHost :: String -> String
checkLocalHost ip =
    if ip == "127.0.0.1"
        then "It's localhost"
        else "No, it's not localhost"


-- Messy if statement
specialBirthdayIf :: Int -> [Char]
specialBirthdayIf age =
    if age == 1
        then "First birthday!"
        else
            if age == 18
                then "You're an adult!"
                else
                    if age == 60
                        then "Finally! I can stop caring about new lingo!"
                        else "Nothing special"


-- Using guards instead of the messy if statement
specialBirthday :: Int -> [Char]
specialBirthday age
    | age == 1  = "First birthday!"
    | age == 18 = "You are now an adult"
    | age == 60 = "Finally! I can stop caring about new lingo!"
    | otherwise = "Nothing special" -- otherwise is the same of using True (just syntatic sugar)


hotterInKelvin :: Double -> Double -> Double
hotterInKelvin c f =
    if c > (f - 32) * 5 / 9
        then c + 273
        else ((f - 32) * 5 / 9) + 273.16


-- let expression to easy readability
hotterInKelvin' :: Double -> Double -> Double
hotterInKelvin' c f =
    let fToC t = (t - 32) * 5 / 9
        cToK t = t + 273.16
        fToK t = cToK (fToC t)
    in if c < fToC f then cToK c else fToK f


-- where expression use here for the same porpouse as the above let example
hotterInKelvin'' :: Double -> Double -> Double
hotterInKelvin'' c f =
    if c > fToC f then cToK c else fToK f
    where
        fToC t = (t - 32) * 5 / 9
        cToK t = t + 273.16
        fToK t = cToK (fToC t)


{-
    Let expressions are convenient whenever we want to split complex expressions
    into smaller building blocks that you combine into a final expression
-}
houseVolume :: Float -> Float -> Float
houseVolume side roofHeight =
    let cubeVolume = side ^ 3
        pyramidVolume = (side ^ 2) * roofHeight / 3
        chimneyVolume = (0.5 ^ 2) * roofHeight
    in cubeVolume + pyramidVolume + chimneyVolume


{-
    Where expressions are convinent whenever we want to scope bindings over
    several guarded equations
-}
analyzeCylinder :: Float -> Float -> String
analyzeCylinder diameter height
        | volume < 10   = "The cylinder is a glass"
        | volume < 100  = "The cylinder is a bucket"
        | volume < 1000 = "The The cylinder is a tank"
        | otherwise     = "What is the world is that huge thing?"
    where
        volume = pi * diameter ^ 2 * height / 4


-- Expressions introduces in a let expression exists only inside that let expression
initials :: String -> String -> String
initials first last =
    if first == "" || last == ""
        then "What is your name again?"
        else
            let x = head first
                y = head last
            in [x] ++ "." ++ [y] ++ "."
