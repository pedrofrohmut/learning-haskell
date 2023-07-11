-- TODO: All of them

{-

**************************** IMPORTANT ****************************

Solve this homework after completing and checking the "Maze" one.

*******************************************************************

We're going to build on top of the "Maze" challenge by coding a similar
but a bit more complicated game.

It works the same as the "Maze" game, with the difference that the player
is now in a forest. Because we're in a forest, there are no walls. And,
if you walk long enough, you're guaranteed to find the exit.

So, what's the challenge in playing this game? The challenge lies in that
now we have "stamina." Stamina is a number (we start with 10). And, each
time the player makes a move, its stamina gets reduced by the amount of work
needed to cross the current trail (represented by a number contained in the
value constructor).

The data types and functions are pretty much the same, with a few caveats:

- We don't have walls.
- We don't want to choose a specific numeric type, but we want to make sure
we can do basic numeric operations regardless of the type we pass to the functions.
- Because now we have to keep track of the player's stamina, we'll need to
move it around with our current forest. This would be an awesome use case
for monads, but because we don't know how to use them yet, a "(stamina, forest)"
pair will have to do.

Using GHCi, like the "Maze" game, this game should look like this:

*Main> solveForest testForest []
"You have 10 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward ]
"You have 7 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward]
"You have 4 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward, GoLeft  ]
"You ran out of stamina and died -.-!"
*Main> solveForest testForest [GoForward, GoLeft , GoRight]
"YOU'VE FOUND THE EXIT!!"
-}


data Move = GoFoward | GoLeft | GoRight deriving (Eq)

type Forest = [Move]
type PlayerMoves = [Move]

{-
    TODO:
    - You start the forest on Point2 (0, 0)
    - The exit is a secret coordinate where y >=0 and x can be any value
    - To be possible to play we use only integer numbers (floating will be almost impossible)
    - The first version of this game will have the same stamina cost for every terrain
    - Maybe add a hint like to tell the player when he is near the exit
    - Make a forest generator
    - Make a debug option so it will show the answer in debug mode
-}
solveForest :: Forest -> PlayerMoves -> String
solveForest forestMap playerMoves
    | null forestMap = "The forest is empty and this adventure is pointless"
    | null playerMoves = "You are now inside the Forest. \nYou have 10 stamina, choose your next move."
    | otherwise = "TODO"
    -- | otherwise = solve forest playerMoves
    -- where
    --     solve forest moves
        -- TODO: make it a recursive function that will match forest and moves head

introStr :: String
introStr =
    "Hello, brave adventurer. You are now inside the Forest!\n\
    \Now you need to provide the forest you choose to claim your victory upon \
    \and the path you will follow.\n\
    \The adventurer path is an array of moves, and you have 3 choices: \
    \GoRight, GoLeft and GoForward.\n\
    \To make I try you have 10 stamina points the you spend to cross the Forest \
    \terrains.\n\
    \Stamina is spent more or less depending on how hard certain terrain is to transpass."

-- Show the secrets and the answer so it is easier to test
debugMode :: Bool
debugMode = True

main :: IO ()
main = do
    putStrLn $ (if debugMode then "# DEBUG MODE #\n" else "") ++ introStr
