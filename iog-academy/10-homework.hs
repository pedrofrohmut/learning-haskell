-- TODO: All of them

{-
-- Question 1 --
Continuing with the logistics software of the lesson:
 1. After using the `Container` type class for a while, you realize that it might
 need a few adjustments:
  	- First, write down the `Container` type class and its instances, same as we
    did in the lesson (try to do it without looking and check at the end or if
    you get stuck).
  	- Then, add a function called `unwrap` that gives you back the value inside
    a container.
 2. Create an instance for the `MailedBox` data type.
 	- The MailedBox data type represents a box sent through the mail.
 	- The parameter `t` is a tag with a person's identifier
 	- The parameter `d` is the person's details (address,etc).
 	- The parameter `a` is the content of the MailedBox
-}

class Container container where
    isEmpty :: container a -> Bool
    contains :: (Eq a) => container a -> a -> Bool
    replace :: container a -> b -> container b
    unwrap :: container a -> a

data Box a = Empty | Has a

instance Container Box where
    isEmpty Empty = True
    isEmpty _ = False

    contains Empty _ = False
    contains (Has x) y = x == y

    replace _ y = Has y

    unwrap (Has x) = x
    unwrap Empty = error "Cannot unwrap Empty"

data Present tag a = EmptyPresent tag | PresentFor tag a

instance Container (Present tag) where
    isEmpty (EmptyPresent _) = True
    isEmpty _ = False

    contains (EmptyPresent _) _ = False
    contains (PresentFor _ x) y = x == y

    replace (EmptyPresent tag) y = PresentFor tag y
    replace (PresentFor tag _) y = PresentFor tag y

    unwrap (PresentFor _ x) = x
    unwrap (EmptyPresent _) = error "Cannot unwrap EmptyPresent"


data MailedBox tag details content
    = EmptyMailBox tag details
    | MailBoxTo taga details content

instance Container (MailedBox tag details) where
    isEmtpy (EmptyMailBox _ _) = True
    isEmpty _ = False

    contains (EmptyMailBox _ _) _ = False
    contains (MailBoxTo _ _ x) y = x == y

    replace (EmptyMailBox tag details) y = MailBoxTo tag details y
    replace (MailBoxTo tag details _) y = MailBoxTo tag details y

    unwrap (MailBoxTo _ _ x) = x
    unwrap (EmptyMailBox _ _) = error "Cannot unwrap EmptyMailBox"

-- Question 2 --
-- Create instances for Show, Eq, and Ord for these three data types (use
-- automatic deriving whenever possible):

data Position = Intern | Junior | Senior | Manager | Chief

data Experience = Programming | Managing | Leading

type Address = String

data Salary = USD Double | EUR Double

data Relationship
  = Contractor Position Experience Salary Address
  | Employee Position Experience Salary Address

data Pokemon = Pokemon
  { pName :: String,
    pType :: [String],
    pGeneration :: Int,
    pPokeDexNum :: Int
  }

charizard = Pokemon "Charizard" ["Fire", "Flying"] 1 6

venusaur = Pokemon "Venusaur" ["Grass", "Poison"] 1 3

-- Question 3 -- EXTRA CREDITS
-- Uncomment the next code and make it work (Google what you don't know).

-- -- Team memeber experience in years
-- newtype Exp = Exp Double
--
-- -- Team memeber data
-- type TeamMember = (String, Exp)
--
-- -- List of memeber of the team
-- team :: [TeamMember]
-- team = [("John", Exp 5), ("Rick", Exp 2), ("Mary", Exp 6)]
--
-- -- Function to check the combined experience of the team
-- -- This function applied to `team` using GHCi should work
-- combineExp :: [TeamMember] -> Exp
-- combineExp = foldr ((+) . snd) 0
