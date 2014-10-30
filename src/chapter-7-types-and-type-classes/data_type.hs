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