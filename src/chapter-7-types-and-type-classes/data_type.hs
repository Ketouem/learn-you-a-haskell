-- Exporting our shapes in a module

-- By using Shape''(..) we export all the value constructors for Shape.
-- This means that people who import our module can make shapes by using
-- the Rectangle and Circle value constructors.
-- Same as writing Shape(Rectangle, Circle).

module Shapes
( Point(..)
, Shape''(..)
, area'
, nudge
, baseCircle
, baseRect
) where

import qualified Data.Map as Map

-- Defining a new data type

-- One way to make our own type is to use the data keyword
-- Ex:

data Bool = False | True

-- Bool is the type itself. The parts after the equal sign are value
-- constructors. They specify the different values the type can have.
-- Both the type name and the value constructors must start with an uppercase
-- letter.

-- Shaping up (create a data type that represents a shape)

-- Let's say a shape can be a circle or a rectangle

data Shape = Circle Float Float Float | Rectangle Float Float Float Float

-- The Circle value constructor has three fields, which take floats.
-- Value constructors are actually functions that ultimately return a value of
-- a data type.

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- We couldn't write Circle -> Float because Circle is not a type, while Shape 
-- is, just like we couldn't write a type declaration True -> Int

-- We can pattern match against constructors.
-- area $ Circle 10 20 10
-- area $ Rectangle 0 0 100 100

-- If we try to print out `Circle 10 20 5` in the prompt we'll get an error.
-- When we want to display value out of a prompt, Haskell first applies the
-- `show` function to it to get the string representation.

-- To make `Shape` part of the Show type class:

data Shape' = Circle' Float Float Float | Rectangle' Float Float Float Float
     deriving (Show)

-- Value constructors are functions hence we can map them, partially apply them
-- etc. Ex: a list of concentric circles with different radii

-- map (Circle 10 20) [4,5,6,6]

-- Improving Shape with the Point Data Type

-- Defining a 2D point

data Point = Point Float Float deriving (Show)
data Shape'' = Circle'' Point Float | Rectangle'' Point Point deriving (Show)

-- Note: when defining a point, we used the same name for the data type and
-- the value constructor, has no special meaning.

area' :: Shape'' -> Float
area' (Circle'' _ r) = pi * r ^ 2
area' (Rectangle'' (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- Moving a shape, the nudge function

nudge :: Shape'' -> Float -> Float -> Shape''
nudge (Circle'' (Point x y) r) a b = Circle'' (Point (x + a) (y + b)) r
nudge (Rectangle'' (Point x1 y1) (Point x2 y2)) a b 
    = Rectangle'' (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

-- We can make some auxiliary functions that can create shapes of some
-- size at the zero origin and then nudge them.

baseCircle :: Float -> Shape''
baseCircle r = Circle'' (Point 0 0) r

baseRect :: Float -> Float -> Shape''
baseRect width height = Rectangle'' (Point 0 0) (Point width height)

-- Record Syntax

data Person = Person String String Int Float String String deriving (Show)

-- Creating functions to get specific pieces of info about a person.

firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname

lastName :: Person -> String
lastName (Person _ lastname _ _ _ _) = lastname

age :: Person -> Int
age (Person _ _ age _ _ _) = age

height :: Person -> Float
height (Person _ _ _ height _ _) = height

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ number _) = number

flavor :: Person -> String
flavor (Person _ _ _ _ _ flavor) = flavor

-- Alternative way to write data types to avoid writing the above functions
-- known as the Record Syntax.

data Person' = Person' { firstName' :: String
                       , lastName' :: String
                       , age' :: Int
                       , height' :: Float
                       , phoneNumber' :: String
                       , flavor' :: String } deriving (Show)

-- Applied on a car

data Car = Car String String Int deriving (Show)

-- ghci> Car "Ford" "Mustang" 1967
-- Car "Ford" "Mustang" 1967

data Car' = Car' { company :: String
                 , model :: String
                 , year :: Int
                 } deriving (Show)

-- ghci> Car' {company="Ford", model="Mustang", year=1967}
-- Car' {company="Ford", model="Mustang", year=1967}
-- This syntax is useful for non trivial objects.

-- Type Parameters

-- Type constructors can take types as parameters and produce new types

-- Example of an already implemented type
data Maybe a = Nothing | Just a
-- `a` is the type parameter. `Maybe` is called a type constructor.
-- Depending on what we want this data type to hold when it's not `Nothing`
-- this type constructor can end up producing a type of `Maybe Int`,
-- `Maybe String`, etc.

-- Works with Haskell's type inference, but if we want to explicitly pass a type
-- as a type parameter using ::
-- ghci> Just 3 :: Maybe Int

-- Type parameters are useful because they allow to make data types that
-- can hold different things
data IntMaybe = INothing | IJust Int
data StringMaybe = SNothing | SJust String
data ShapeMaybe = ShNothing | SHJust Shape
-- We could even use type parameters to make a generic Maybe that can contain values
-- of any type at all

-- Vector von Doom

-- Implementing a 3D vector that holds numeric types, but must be of same data type
-- part before the = sign -----> type constructor
-- part after the = sign ------> value constructors (possibly separated by |)
data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

-- Derived Instances

-- =/= OOP, we first make the data type then think about how it can act

-- Equating People

data Person'' = Person'' { firstName'' :: String
                         , lastName'' ::String
                         , age'' :: Int
                         } deriving (Eq)
-- When we derive the Eq instance for a type and then try to compare two values of
-- that type with == or /=. Warning: the types of all the field also must be part of the
-- Eq type class

mikeD = Person'' {firstName'' = "Michael", lastName'' = "Diamond", age'' = 43}
adRock = Person'' {firstName'' = "Adam", lastName'' = "Horowitz", age'' = 41}
mca = Person'' {firstName'' = "Adam", lastName'' = "Yauch", age'' = 44}

-- Since Person'' is now in the Eq type class it can be used for all the functions that
-- have a class constraint of Eq.
-- ghci> let beastieBoys = [mca, adRock, mikeD]
-- ghci> mikeD `elem` beastieBoys
-- True

-- Show me how to Read

-- Show and Read type classes are for things that can be converted to or from strings,
-- respectively.

data Person''' = Person''' { firstName''' :: String
                           , lastName''' ::String
                           , age''' :: Int
                           } deriving (Eq, Show, Read)

-- ghci> "mikeD is: " ++ show mikeD
-- "mikeD is: Person''' {firstName''' = \"Michael\", lastName''' = \"Diamond\", age''' = 43}"
-- ghci> read "Person''' {  firstname''' = \"Michael"" ++
--                       ", lastname''' = \"Diamond\"" ++
--                       ", age''' = 43}" :: Person'''

-- Order in the court

-- We can derive instances for the Ord type class, which is for types that have values that can
-- be ordered

-- data Bool = False | True deriving (Ord)

-- ghci> True `compare` False
-- GT
-- ghci> True > False
-- True
-- ghci > True < False
-- False

--Any day of the week

-- Defining an enum, all the type's value constructor are nullary (don't have any fields)
-- Enum type class is for things that have predecessors and successors.
-- Bounded can also be used, which is for things that have a lowest and highest possible value.
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Thanks to Bounded
minBoundDay = minBound :: Day
maxBoundDay = maxBound :: Day

-- Thanks to Enum
succMonday = succ Monday
predSaturday = pred Saturday
sliceDays = [Thursday .. Sunday]
sliceAllDays = [minBound .. maxBound] :: [Day]

-- Type synonyms

-- [Char] ans String types are equivalent. They are implemented with type synonyms.
-- type String = [Char]

-- Making our phonebook prettier

-- Old version
phoneBook :: [(String, String)]
phoneBook = [("betty", "555-2938")]

-- Let's use type synonyms to make the whole thing more expressive
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

-- Useful to use synonyms on String when we want to convey some meaning
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Prelude.Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

-- Same without type synonyms
-- inPhoneBook :: String -> String > [(String, String)] -> Bool

-- Parameterizing type synonyms
type AssocList k v = [(k, v)]

-- Just as we can partially apply functions to get new functions, we can partially apply
-- type parameters and get new type constructors from them.
-- e.g. if we wanted a type that represents a map from integers to something, we could do this
-- type IntMap v = Map.Map Int v
-- or type IntMap = Map Int

-- Type synonyms (and types generally) can be used onyl in the type portion of Haskell.
-- Type portion includes data and type declaration, as well as after a :: in type declarations or
-- type annotations.

-- Go Left, Then Right

-- Another data type that takes two types as its parameters is `Either a b` type
-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
-- 2 value constructors. If `Left` is used, then its content are of type a, resp `Right` and b.

-- We can use this type to encapsulate a value of one type or another. Then when we get a value of
-- type `Either a b`, we usually pattern match on both Left and Right.

-- So far we used `Maybe a` to represent the results of computations that could have failed.
-- But sometimes it is not enough because `Nothing` doesn't convey much info other than that sth
-- has failed.
-- When we're interested in how or why a function failed, we usually use the result type of
-- `Either a b` where `a` is the failure type and `b` the type of a successful computation.

-- Ex: each locker has a code combination. When students need to be assigned a locker, they tell
-- the locker supervisor which locker number they want, and he gives thde code. However of the locker
-- is already being used, the student needs to pick a different one.

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

-- We'll make a function that searches for the code in a locker map. We'll use an `Either String Code`
-- type to represent the result, because the lookup can fail in two ways:
--   1) the locker can be taken
--   2) the locker number might not exist

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
  Prelude.Nothing -> Left $ "Locker" ++ show lockerNumber ++ " doesn't exist!"
  Prelude.Just (state, code) -> if state /= Taken
                          then Right code
                          else Left $ "Locker " ++ show lockerNumber
                                      ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
  [(100, (Taken, "ZD39I"))
  ,(101, (Free, "JAH3I"))
  ]

-- Recursive Data Structures

-- e.g. the List
-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- Note Cons <--> :

-- Improving our list

-- We can define function to be automatically infix by naming them using only special characters.
-- We can also do the same with constructors, since they're just functions that return a data type.
-- There is one restriction however: infix constructors must begin with a colon.

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

-- New syntactic declaration `fixity`. A fixity states how tightly the operator binds and whether
-- it's left/right-associative. Fixity value is optional.

-- Let's re-implement adding list together (++)
infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)
-- We pattern matched on (x :-: xs). That works because pattern matching is actually about matching constructors.

-- Let's Plant a Tree

-- Binary search tree, left elt is smaller, right is bigger
-- A tree is either an empty tree or it's an element that contains some value and two trees. Perfect fit for ADT.
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

-- Building the tree
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree
-- singleton is a shortcut for creating a node that has something set as its root and 2 empty subtrees.

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a  = Node a (treeInsert x left) right
  | x > a  = Node a left (treeInsert x right)

-- A function that checks if some element is in the tree.
treeElem :: (Ord a) => a -> Tree a -> Prelude.Bool
treeElem x EmptyTree = Prelude.False
treeElem x (Node a left right)
  | x == a = Prelude.True
  | x < a  = treeElem x left
  | x > a  = treeElem x right

-- We'll use a fold to build a tree from a list. Note: pretty much everything that traverses a list one item at a time
-- and returns a value can be implemented with a fold.

nums = [8, 6, 1, 4, 7, 3, 5]
numsTree = foldr treeInsert EmptyTree nums

-- Type classes 102

-- Recap: Type classes are sort of like interfaces. A type class defines some behavior (such as comparing for equality).
--        Types that can behave in that way are made instances of that type classes. The behavior of type classes is
--        achieved by defining functions or just type declarations that we then implement. So when we say that a type is an
--        instance of a type class, we mean that we can use the functions that the type class defines with that type.