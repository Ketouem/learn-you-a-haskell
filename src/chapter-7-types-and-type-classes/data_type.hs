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
