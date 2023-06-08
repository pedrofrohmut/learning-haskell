-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.
-- (Use the types presented in the lecture.)

f1 :: Float -> Float -> Float -> Float
f1 x y z = x ** (y/z)


f2 :: Float -> Float -> Float -> Float
f2 x y z = sqrt (x/y - z)


f3 :: Bool -> Bool -> [Bool]
f3 x y = [x == True] ++ [y]


f4 :: [Float] -> [Float] -> [Float] -> Bool
f4 x y z = x == (y ++ z)


-- Question 2
-- Why should we define type signatures of functions? How can they help you?
-- How can they help others?

answer2 =
    " it is a way to express what the function expects as parameters and what the\
    \ type it returns. And helps to identify author intenssion and use case "

-- Question 3
-- Why should you define type signatures for variables? How can they help you?

answer3 =
    " 1. For variables can be used to get compile time errors on simple stuff \
    \ 2. Can also be used to tell other devs what your intenssions with a variable \
    \ The benefits are bigger than the time you spend to type them "

-- Question 4
-- Are there any functions in Haskell that let you transform one type to the other?
-- Try googling for the answer.

answer4 = "Numeric: https://wiki.haskell.org/Converting_numbers"

-- Question 5
-- Can you also define in Haskell list of lists? Did we showed any example of that?
-- How would you access the inner most elements?

myList :: [Int]
myList = [1, 2, 3, 4, 5, 6, 7]


secondElem = myList !! 1

mat :: [[Char]]
mat = [ ['f', 'o', 'o']
      , ['b', 'a', 'r']
      , ['b', 'a', 'z']
      ]

elem12 = mat !! 0 !! 1 -- decrease 1 because lists indexes start at 0
