{-
    Outline

    - Parameterizing Types
        - Parameterizing 'type' synonyms
        - Parameterizing 'date' types
    - Recursive data types
        - Tweet me a river
        - A Sequence of Nodes
        - A Tree of Nodes
    - Kinds
    - The newType keyword
-}

{-
    Parameterizing Type Synonyms
-}
type Name = String
type Address = (String, Int)
type CompanyId = Int
type ProviderId = String

{-  The three structs have Address as a common element

    type Person = (Name, Address)
    type Company = (CompanyId, Address)
    type Provider = (ProviderId, Address)
-}

-- Using Type Synonyms to pass a type as parameter to create another type
type Entity a = (a, Address)

type Person = Entity Name
type Company = Entity CompanyId
type Provider = Entity ProviderId

{-
    Parameterizing Data Types
-}

-- A box that contains a or is Empty
data Box a = Empty | Has a deriving (Show)

box1 = Has "What's is the box?"
box2 = Empty
box3 = Has (1 :: Int)

addN :: Num a => a -> Box a -> Box a
addN _ Empty = Empty
addN n (Has a) = Has (a + n)

has3 = addN 2 box3 -- Has 3

addBoxes :: Num a => Box a -> Box a -> Box a
addBoxes _ Empty = Empty
addBoxes Empty _ = Empty
addBoxes (Has x) (Has y) = Has (x + y)

has7 = addBoxes (Has 4) (Has 3) -- Has 7

-- With case-of and let-expression
addBoxes' :: Num a => Box a -> Box a -> Box a
addBoxes' x y =
    let
        tuple = (x, y)
    in
        case tuple of
            (_, Empty)         -> Empty
            (Empty, _)         -> Empty
            ((Has a), (Has b)) -> Has (a + b)

-- With case-of and where
addBoxes'' :: Num a => Box a -> Box a -> Box a
addBoxes'' x y =
    case tuple of
        (_, Empty)         -> Empty
        (Empty, _)         -> Empty
        ((Has a), (Has b)) -> Has (a + b)
    where
        tuple = (x, y)

extract :: a -> Box a -> a
extract dft Empty = dft
extract _ (Has a) = a

ext1 = extract 'a' Empty -- 'a'
ext2 = extract 0 (Has 15) -- 15
ext3 = extract 0 Empty -- 0
ext4 = extract [] (Has [1,2,3,4]) -- [1,2,3,4]

data Shape a
    = Circle
        { position :: (Float, Float)
        , radius :: Float
        , color :: a
        }
    | Rectangle
        { position :: (Float, Float)
        , height :: Float
        , width :: Float
        , color :: a
        }
    deriving (Show)

-- Circle with String
circleStr = Circle { position = (1,2), radius = 6, color = "Red" }

type RGB = (Int, Int, Int)
-- Circle with RBG tuple
circleRGB = Circle { position = (1,2), radius = 5, color = (0, 128, 0)::RGB }

{-
    Recursive Data Types
-}

data Tweet = Tweet
    { contents :: String
    , likes :: Int
    , comments :: [Tweet]
    }
    deriving (Show)

tweet :: Tweet
tweet =
    Tweet "I'm angry about something! >.<" 5
        [ Tweet "Me too!" 0 []
        , Tweet "It takes me angry that you are angry" 2
            [ Tweet "I have no idea what's happening" 3 [] ]
        ]

engagement :: Tweet -> Int
engagement Tweet { likes=l, comments=c } = l + length c + sum (map engagement c)

thisEngagement = engagement tweet

{-
    Write a data type that represents a linear sequence of nodes where each node
    contains a value and points to the rest of the sequence
-}

-- data Box a = Empty | Has a (Box a)
data Sequence a = EmptyNode | Node a (Sequence a) deriving (Show)

sequence1 :: Sequence a
sequence1 = EmptyNode -- A sequence of just one empty node

sequence2 :: Sequence Char
sequence2 = Node 'a' EmptyNode -- sequence of 2 nodes

sequence3 :: Sequence Bool
sequence3 = Node True (Node False EmptyNode) -- sequence of 3 nodes

sequence4 :: Sequence Integer
sequence4 = Node 1 (Node 2 (Node 3 EmptyNode)) -- sequence of 4 nodes

-- infixr is the same value of the cons operator ':' used in lists
infixr 5 :->
data Sequence' a = EmptySq | a :-> (Sequence' a) deriving (Show)

sequence5 :: Sequence' Integer
sequence5 = 1 :-> 2 :-> 3 :-> 4 :-> EmptySq -- a sequence of 4 + emptyNode

-- The list type for comparison
sequence6 :: [] Integer
sequence6 = 1 : 2 : 3 : 4 : [] -- a sequence of 4 + emptyNode

{-
-- Our list like type And The list the way is defined in Haskell

    data Sequence' a = EmptySq | a :-> (Sequence' a)
    data []       a = []        | a :   [a]
-}

hasElemInSeq :: (Eq a) => a -> Sequence' a -> Bool
hasElemInSeq  _ EmptySq   = False
hasElemInSeq x (y :-> ys) = if x == y then True else hasElemInSeq x ys

sequence7 :: Sequence' Char
sequence7 = 'a' :-> 'b' :-> '4' :-> '%' :-> EmptySq

has6 = hasElemInSeq '6' sequence7
hasB = hasElemInSeq 'b' sequence7

{-
    Binary Tree
-}
data Tree a = EmptyTree | TreeNode a (Tree a) (Tree a) deriving (Show)

emptyTree :: Tree a
emptyTree = EmptyTree

oneLevelTree :: Tree Char
oneLevelTree = TreeNode 'a' EmptyTree EmptyTree

twoLevelTree :: Tree Integer
twoLevelTree =
    TreeNode 8
        (TreeNode 3 EmptyTree EmptyTree)
        (TreeNode 10 EmptyTree EmptyTree)

threeLevelTree :: Tree Integer
threeLevelTree =
    TreeNode 8
        (TreeNode 3
            (TreeNode 1 EmptyTree EmptyTree)
            (TreeNode 6 EmptyTree EmptyTree)
        )
        (TreeNode 10
            EmptyTree
            (TreeNode 14 EmptyTree EmptyTree)
        )

-- built-in 'elem' checks if a elem is in a list and we will follow the same name
-- convention to check our tree

-- The elem func that check all the list from left to right (linear search style)
elemTree :: Ord a => a -> Tree a -> Bool
elemTree _ EmptyTree = False
elemTree val (TreeNode y treeL treeR) =
    val == y || elemTree val treeL || elemTree val treeR

-- Otimization: follows binary search style
elemTree' :: Ord a => a -> Tree a -> Bool
elemTree' _ EmptyTree = False
elemTree' val (TreeNode y treeL treeR)
    | val == y = True
    | val > y  = elemTree val treeR
    | val < y  = elemTree val treeL

-- The shape of the data type directs how you write functions with it

{-
    Kind - The type of a type

    The 'type' of a value constructor gives you the quantity and type of the values
    it takes.

    The 'kind' of a type constructor gives you the quantity and kind of types it takes

    - '*' means Concrete Type (a type that doens't take any parameters. Like Float)
    - '* -> *' means type constructor that takes a single  concrete type and returns
    another concrete type (Like: Box a)
    - '* -> (* -> *) -> *' means type constructor that takes one concrete type,
    and one single-parameter constructor, and returns a concrete type (we haven't
    seen one yet)

    :kind Int    -- > Int::*
    :kind String -- > String::*
    :kind Bool   -- > Bool::*

    :kind Box      -- > Box::*->*
    :kind Sequence -- > Sequence::*->*
    :kind Tree     -- > Tree::*->*

    -- Entity a b = (a, b)
    :kind Entity -- > Entity::*->*->*
-}

{-
    The newType keyword

    Type created with newType need to have exactly one constructor with exactly
    one parameter/field

-- Like this:
    newType Color a = Color a

-- And this:
    newType Product a = Product { getProduct :: a }
-}
