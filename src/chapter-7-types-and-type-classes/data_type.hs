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