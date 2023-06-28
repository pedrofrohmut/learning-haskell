{-
    Outline:

    - Pure functions
    - Introduction to IO actions
    - IO actions under the hood
    - IO actions in practice
        - The () type
    - Interacting with the user
        - getChar, getLine and putStrLn
    - Actions are first-class values
    - Composing IO actions (>> and >>= operators)
    - The do block
        - Using 'let', nesting 'do-blocks', escaping 'IO' and 'return'
    - The 'main' action
    - Recap
-}

{-
    Pure functions

    Input -> Function -> Output

    The Output depends only on the values passed as Input to the Function

    With the same output a pure function always returns the same output

    tripleMe :: Num a => a -> a
    3 -> tripleMe x = x * 3 -> 9

    Every time you input 3 you get 9

    filterOdds :: [Int] -> [Int]
    [1,2,3,4] -> filterOdds = filter odd -> [1,3]
-}

{-
    Introduction to IO Actions:

    IO Action (or just action/computation) can interact with and change the world
    outside our program.

    Side Effects: a side effect is any observable effect other than returning a
    value.

    Examples:

    - Obtain the result of what a person typed in the keyboard.

    IO action/Result:
        Interact with the keyboard <- -> [  ] -> Text typed

    - Show some text, an image, or something on the screen.

    IO action/Result:
        Show on screen <- -> [  ] -> Nothing (or placeholder value)

    - Call to an API or a database.

    IO action/Result:
        Call an API or Database <- -> [  ] -> Info Return by API or Database
-}

{-
    IO actions under the hood:

    newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

    it takes the State of the whole RealWorld, do something with it, and returns
    the new State of the RealWorld and the parameter passed to this function

    Abstract Data Types: is a type whose the representation is hidden, but it
    provides associateded operations.

    We don't use the IO constructor directly. We use functions and actions that
    operate with IO values.
-}

{-
    IO Actions in practice:

    IO <- -> [ function :: IO a ] -> a

    action1 :: IO Bool             -- Performs IO and returns a Bool
    action2 :: IO Int              -- Performs IO and returns a Int
    action3 :: IO (Double -> Char) -- Performs IO and returns a function
    action4 :: IO ()               -- Performs IO and returns ()

    The unit data type: it is a data type that takes no arguments and returns a unit

    data () = ()

    This data type is useless with pure functions but is usefull when dealing with
    side effects
-}

{-
    Interacting with the real world: getChar, getLine and putStrLn

    getChar:
        User type a char -> [ getChar :: IO Char ] -> Char

    getLine:
        User type a line of text -> [ getLine :: IO Char ] -> Char

    putStrLn:
        String -> [ putStrLn :: IO () ] -> IO ()
-}

{-
    Actions are first-class values
-}

-- It does no perform the action. It returns an action and store it on 'x'
-- The action can be later performed calling 'x'
x = putStrLn "Hello, World!"

-- You can store them in a list, just like any data type
listOfActions :: [IO ()]
listOfActions = [putStr "a", putStrLn "b"]

-- You can pass them as function parameters
fakeLength :: [IO ()] -> Int
fakeLength list = 1 + length list

{-
    Composing IO actions (>> and >>= operators)

    (>>) :: IO a -> IO b -> IO b
-}

rudeBot :: IO ()
rudeBot = putStrLn "Hey!"

pureThen :: p1 -> p2 -> p2
pureThen _ x = x

-- The '>>' operator sequentially composes two actions, discarting any values
-- produced by the first
abc :: IO ()
abc = putStrLn "a" >> putStrLn "b" >> putStrLn "c"

-- Like in the pureThen function, it performs putStrLn "a" and discart the result
-- performs the putStrLn "b" and discart the result and then performs the
-- putStrLn "c" and returs the result of it

-- The bot is even ruder
rudeBot2 :: IO ()
rudeBot2 = putStrLn "Hey!" >> putStrLn "Get out of my lawn!"

-- The bot get crazy rude now
rudeBot3 :: IO ()
rudeBot3 =
    putStrLn "What is your name?"        -- IO ()
    >> getLine                           -- IO String
    >> putStrLn "like I care! Get lost!" -- IO ()

{-
    (>>=) :: IO a -> (a -> IO b) -> IO b
-}

yellIt :: String -> IO ()
yellIt str = putStrLn (str ++ "!!!")

yellItBack :: IO ()
yellItBack = getLine >>= yellIt

response :: String -> IO ()
response name = putStrLn $ "Nice to meet you " ++ name ++ "."

-- Bot went to therapy
rudeBot4 :: IO ()
rudeBot4 =
    putStrLn "What's your name?"
    >> getLine
    >>= response

countLettersInName :: String -> String
countLettersInName name = "Your name has " ++ (show $ length name) ++ " letters"

chatBot :: IO ()
chatBot =
    putStrLn "Hello. What's your name?"
    >> getLine
    >>= (\name ->
        putStrLn ("Nice to meet you " ++ name ++ ".")
        >> putStrLn (countLettersInName name)
    )

finalChatBot :: IO ()
finalChatBot =
    putStrLn "Hi. What's your name?"
    >> getLine
    >>= (\name ->
        putStrLn ("Nice to meet you, " ++ name ++ ".")
        >> putStrLn ("Your name has " ++ (show $ length name) ++ " letters.")
        >> putStrLn ("So, " ++ name ++ ". What do you do for fun?")
        >> getLine
        >>= (\hobby ->
            putStrLn ("Are you kidding, " ++ name ++ "?! I love " ++ hobby ++ " too!")
        )
    )

{-
    To 'do' notation

    is syntax sugar for the >> and >>= operations

    instead of using the flow left to right, we change to a flow top to bottom
-}

rudeBot' :: IO ()
rudeBot' = do
    putStrLn "Hey!"

rudeBot2' :: IO ()
rudeBot2' = do
    putStrLn "Hey!"
    putStrLn "Get out of my lawn!"

-- The bot get crazy rude now
rudeBot3' :: IO ()
rudeBot3' = do
    putStrLn "What is your name?"     -- IO ()
    getLine                           -- IO String
    putStrLn "like I care! Get lost!" -- IO ()

-- -- Bot went to therapy
rudeBot4' :: IO ()
rudeBot4' = do
    putStrLn "What's your name?"
    -- <- (to left arrow) binds the result of an action to a variable
    name <- getLine
    putStrLn $ "Nice to meet you " ++ name ++ "."

chatBot' :: IO ()
chatBot' = do
    putStrLn "Hello. What's your name?"
    name <- getLine
    putStrLn $ "Nice to meet you " ++ name ++ "."
    putStrLn $ countLettersInName name

finalChatBot' :: IO ()
finalChatBot' = do
    putStrLn "Hi. What's your name?"
    name <- getLine
    putStrLn $ "Nice to meet you, " ++ name ++ "."
    putStrLn $ "Your name has " ++ (show $ length name) ++ " letters."
    putStrLn $ "So, " ++ name ++ ". What do you do for fun?"
    hobby <- getLine
    putStrLn $ "Are you kidding, " ++ name ++ "?! I love " ++ hobby ++ " too!"

{-
    Using 'let' inside the 'do' notation
-}
unscramble :: IO ()
unscramble = do
    putStrLn "Write a bunch of numbers and letters scrambled together"
    arg <- getLine
    let numbers = filter (`elem` "1234567890") arg
        letters = filter (`notElem` numbers) arg
    putStrLn $ "Numbers: " ++ numbers
    putStrLn $ "Letters: " ++ letters

{-
    Nesting 'do' blocks
-}

plusADecade :: IO ()
plusADecade = do
    putStrLn "What is your age?"
    ageString <- getLine
    let validAge = all (`elem` "1234567890") ageString
    if validAge
        then do
            let age = read ageString :: Int
            putStrLn $ "In 10 years you will be " ++ (show $ age + 10) ++ " years old."
        else do
            putStrLn "Your age should contain only digits from 0-9. (Press Ctrl+C to close)"
            plusADecade

{-
    Escaping 'IO' and the return function

    !! In Haskell return doesn't work like the other programming languages. It's
    a different thing. !!
-}
twice :: IO String
twice = do
    str <- getLine
    let tw = str ++ str
    return tw

-- No Syntatic Sugar
twice' :: IO String
twice' =
    getLine
    >>= \str -> let tw = str ++ str in return tw

{-
    The 'main' action.

    The entry point of a Haskell application.

    main :: IO ()
    main = putStrLn "Hello, World!"

    but if the entry point is not pure how to keep your code pure?

    The most common strategy is to make pure functions that are called by the
    impure functions. And, keep most of your code in this pure functions and
    make the impure part of your code as small as possible.

    Example:

    main :: IO ()
    main = do
        config <- getConfig              -- Custom IO
        let result = pureFunction config -- Huge pure function
        putStrLn $ "Done! The result is: " ++ show result
-}

{-
    Pure vs Impure:

    1. Parameters and result: with the same parameters pure always produce the
    same result and impure may produce different results.

    2. Side effects: Pure never has side effect and impure may have them.

    3. State: Pure never alters any state and impure may alter the state of the
    app or the outside environment.

    4. IO: Pure doesn't have IO and impure may have it.

    5. Behavior: Pure have an expected and easy to test behavior, but impure have
    an impredictable behaviour that may depend on the app state, IO devices, the
    current date, the result of some API, database queries, etc.
-}

{-
    Syntax Recap:

    '>>' Sequentially composes two actions, discarting the result of the first.

    '>>=' Sequentially composes two actions, forwarding the result of the first to the second.

    'return' Function that injects a value inside a context.

    'do' Start a do block.

    '<-' Inside the do block: Bind result of performing an action.

    'let' Inside the do block: Lazy binding of any expression (it doesn't perform actions).

    'main' The entry point of a Haskell program (All functions/actions end in main).
-}

--------------------------------------------------------------------------------

assert :: Bool -> Bool -> String -> String
assert exp res msg
    | exp && res || not exp && not res = msg ++ " passed"
    | otherwise                        = "Failure for: " ++ msg

main :: IO ()
main = do
    putStrLn $ assert ((fakeLength listOfActions) == 3) True "Test1"
    putStrLn $ assert ((3 `pureThen` 5) == 5) True "Test2"
    putStrLn $ assert ((3 `pureThen` 5 `pureThen` 6 `pureThen` 'a') == 'a') True "Test3"
