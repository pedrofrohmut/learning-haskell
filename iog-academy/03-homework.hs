-- Question 1
-- Write a function that checks if the monthly consumption of an electrical
-- device is bigger, equal, or smaller than the maximum allowed and returns a
-- message accordingly.
-- The function has to take the hourly consumption of an electrical device, the
-- hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).

isConsumptionAllowed :: Float -> Float -> Float -> String
isConsumptionAllowed consumptionInKw dailyUseInHour monthLimit =
    if monthlyUsage >= monthLimit
        then "It exceed the limit for this month"
        else "The consumption is below the allowed amount"
    where
        dailyUsage   = consumptionInKw * dailyUseInHour
        monthlyUsage = dailyUsage * 30


-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.

-- In the previous function, return the excess/savings of consumption as part of the message.

isConsumptionAllowed' :: Float -> Float -> Float -> String
isConsumptionAllowed' hourlyConsuptionInKw dailyUseInHour monthAllowedKw =
    if monthlyUsage >= monthAllowedKw
        then "It exceeds the limit by " ++ show excess
        else "This consumption is allowed"
    where
        dailyUsage   = hourlyConsuptionInKw * dailyUseInHour
        monthlyUsage = dailyUsage * 30
        excess       = monthlyUsage - monthAllowedKw


-- Question 3
-- Write a function that showcases the advantages of using let expressions to
-- split a big expression into smaller ones. Then, share it with other students in Canvas.


-- Question 4
-- Write a function that takes in two numbers and returns their quotient such
-- that it is not greater than 1. Return the number as a string, and in case the
-- divisor is 0, return a message why the division is not possible. To implement
-- this function using both guards and if-then-else statements.

division :: Float -> Float -> String
division dividend divisor =
    if divisor == 0
        then "Cannot devide by zero"
        else
            let quotient = (dividend / divisor)
                result   = floor quotient
            in show result


division' :: Float -> Float -> String
division' x y
        | y == 0    = "Cannot devide by zero"
        | otherwise = show result
    where
        result = x / y


-- Question 5
-- Write a function that takes in two numbers and calculates the sum of squares
-- for the product and quotient of those numbers. Write the function such that
-- you use a where block inside a let expression and a let expression inside a
-- where block.

myFunction :: Float -> Float -> Float
myFunction x y =
        result
    where
        product  = x * y
        quotient = x / y
        result   = product^2 + quotient^2
