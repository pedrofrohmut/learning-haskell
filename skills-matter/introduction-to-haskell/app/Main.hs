module Main (main) where

import Lib

hello1 :: IO ()
hello1 = do
    putStrLn "What your first name?"
    firstName <- getLine
    putStrLn "What is your last name?"
    lastName <- getLine
    putStrLn ("Nice to meet you " <> firstName <> " " <> lastName)


whoAreYou :: IO String
whoAreYou = do
    putStrLn "Who are you?"
    getLine


hello2 :: IO ()
hello2 = do
    name1 <- whoAreYou
    name2 <- whoAreYou
    putStrLn
        ("Nice to meet you " <> name1 <> " and  " <> name2)


questions :: [String]
questions
    ["Who are you?", "Are you a Haskeller yet?"]


prompts :: [IO String]
prompts
    map prompt questions


askQuestions :: IO [String]
askQuestions =
    sequence prompts


-- define a struct called Dialogue that can have two forms
-- Ask with string and 2 dialogues or Done with only a string
-- This struct functions as a tree nodes of a binary tree
data Dialogue =
        Ask String Dialogue Dialogue
    |   Done String


haskellConversation :: Dialogue
haskellConversation =
    Ask "Are you also at HaskellX?"
        (Done "Oh, too bad.")
        (Ask "Are you a haskeller yet?"
            (Done "Perhaps after this day.")
            (Done "That's great.")
        )
