multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z
-- Type delcaration of this function could also be written as
-- multThree :: Int -> (Int -> (Int -> Int))
-- When called function that takes multiple parameters
-- returns a partially applied function

compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

-- Sections: infix functions can be partially applied by using sections. Surround
-- the infix function with parameter, that creates a function that takes one parameter
-- and then applies it to the side that' missing.
divideByTen :: (Floating a) => a -> a
divideByTen = (/10) --same as (/10) applied on a

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- WARNING: section and negative operator (-4) == -4 (for convenience)
--          to create a section that substracts for: (substract 4)

-- Higher orderism
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- Example of use cases:
-- applyTwice (+3) 10
-- applyTwice (++ " HAHA") "HEY"
-- applyTwice (3:) [1]

-- Re-implementing zipWith
-- It takes a function and two lists as parameters and then joins them by
-- applying the given function

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
-- First argument is a function that takes two args and return one value
-- 2nd & 3rd parameters are list
-- Returns a list
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Pro-tip: when writing a high order function (or a simple function)
-- and unsure of the type, the function can be written without the type
-- declaration and checking the inferred type with :t under ghci

-- Re-implementing flip
-- It takes a function and returns a function that is like the original one
-- but with the first two arguments flipped

-- flip' :: (a -> b -> c) -> (b -> a -> c)
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = g
  where g x y = f y x

-- Functional programmer's toolbox

-- map : takes a function and a list and apply that function to every item
--       in the list producing a new list
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- filter : takes a predicate and a list and returns the lists of elements
--          that satisfy that predicate
-- Note: a predicate is a function that returns a Boolean value
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x: filter' p xs
  | otherwise = filter' p xs

-- Another implementation of the chapter 4 quicksort function that uses filter
-- Reminder: the fat arrow is used to define precondition
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerOrEqual = filter (<= x) xs
      larger = filter (> x) xs
  in quicksort smallerOrEqual ++ [x] ++ larger

-- More examples of map and filter

-- Ex: let's find the largest number under 100000 that is divisble by 3829
largestDivisible :: Integer
largestDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0
-- Haskell's laziness causes the evaluation to stop when the first adequate
-- solution is found

-- Ex: find the sum of all odd squares that are smaller than 10000
sumOddSquares :: Integer
sumOddSquares = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- Alternate version with list comprehension
sumOddSquares' :: Integer
sumOddSquares' = sum (takeWhile (<10000) [m | m <- [n^2 | n <- [1..]], odd m])

-- Ex: Collatz sequence
-- * Start with any natural number
-- * If the number is 1 stop
-- * If the number is even, divide it by 2
-- * If the number is odd, multiply it by 3 and add 1
-- * Repeat the algorithm with the resulting number
-- For all starting number between 1 and 100, how many Collatz chains have a
-- lenght greater than 15.
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

-- Mapping functions with multiple parameters
listOfFuns = map (*) [0..]
-- Extracting the function at the index 4 and using it
multiplyByFour = listOfFuns !! 4

-- Lambdas
-- Are anonymous functions that we use when we need a function only once.

-- To write a lambda:
-- Start with \
-- Parameters separated by spaces
-- A ->
-- The function body
-- Note: Usually lambdas are surrounded by parentheses
--       When a lambda is not surrounded by parentheses, it assumes that
--       everything to the right of the -> belongs to it.

numLongChains' :: Int
numLongChains' = length (filter (\ xs -> length xs > 15) (map chain [1..100]))

-- Like normal functions, lambdas can have any number of parameters
-- Can pattern match, only difference is that we can't define several patterns
-- for one parameter

-- Folds
-- Allow to reduce a data structure, like a list, to a single value
-- Folds can be used to implement any functions where you traverse a list once,
-- element by element, and then return an element based on that.

-- A fold takes:
-- * A binary function (function that takes two parameters)
-- * A starting value (accumulator)
-- * A list to fold up
-- Can fold from L to R (foldl) or R to L (foldr)

-- Left fold
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
-- We can write it even more succently by currying +
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0 -- Sends back a function that takes a parameter

-- Reminder: generally, when we have a function like `foo a = bar b a` it can be
--           rewritten as `foo = bar b` because of currying

-- Right fold
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- (!) :  Right folds work on infinite list whereas Left ones do not !

-- Reimplementation of elem, that checks if an element is in a list
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

-- foldl1 & foldr1 functions
-- Works like foldl and foldr except that we don't need to provide them explicit
-- starting accumulator. They assume the first (or last) element of the list to
-- be the accumulator
maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

-- Pro Tip: when making a fold, if the function does not make sense when given
--          an empty list, we can probably use a foldl1 or foldr1 to implement
--          it

-- Some fold examples

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []
-- Another trickier version
reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- Folding inifinite lists

-- Reimplentation of and, takes a list of Booleans and sends back True if all
-- elements are True, False otherwise

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

-- and' [True, False, True] <=> True && (False && (True && True))
-- the last True represents the starting accumulator

-- foldr will work on infinite lists when the binary function that we're passing
-- doesn't always need to evaluate its second parameter to give us some sort of
-- answer (like &&)

-- Scans
-- scanl and scanr functions are like foldl and foldr except they report all the
-- intermediate accumulator states in the form of a list. scanr1 & scanl1 are
-- analogous to foldl1 and foldr1

-- Ex:
-- scanl (+) 0 [3,5,2,1]
-- >>> [0,3,8,10,11]
-- scanr (+) 0 [3,5,2,1]
-- >>> [11,10,8,3,0]

-- Scans are used to monitor the progress of a function that can be implemented
-- as a fold

-- Ex: find how many elements does it take for the sum of the square roots of
--     all natural numbers to exceed 1000

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- takeWhile is used instead of filter because filter wouldn't cut off the
-- resulting list once a number that is equal or greater than 1000 is found.

-- Function application with $

-- ($) :: (a -> b) -> a -> b
-- f $ x = f x

-- Normal function application (putting a space between two things) has a really
-- high precedence, the $ application has the lowest.

-- Normal function application is left associative: f a b c <=> ((f a) b) c
-- $ function application is right associative: f $ g $ x <=> f $ (g $ x)

-- Most of the time it is a convenience function that lets us write fewer
-- parentheses
-- When a $ is encountered, the expression on its right is applied as the
-- parameter to the function on its left.

-- sqrt 3 + 4 + 9 : adds 4 and 9 then adds it to the square root of 3
-- sqrt (3 + 4 + 9) : square root of the whole sum
-- can also be written as sqrt $ 3 + 4 + 9

-- Ex: sum (filter (>10) (map (*2) [2..10]))
--     equivalent to
--     sum $ filter (>10) (map (*2) [2..10])
--     equivalent to
--     sum $ filter (>10) $ map (*2) [2..10]

-- Apart from getting rid of parentheses, $ lets us treat function application
-- like just another function.
-- Ex: map a function application over a list of functions
--     map ($ 3) [(4+), (10*)]
--     >>> [7.0, 30.0]

-- Function composition
-- (f°g)(x) = f(g(x))

-- In Haskell, we do function composition with the . function, which is defined
-- like this
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)

-- One use for function composition is making functions on the fly to pass to
-- other functions (we could use lambdas but is often more clearer and concise
-- to use composition).

-- Ex: function that takes a list of number and turn them into negative number
negateAll :: (Num a) => [a] -> [a]
negateAll xs = map (\x -> negate (abs x)) xs

negateAll' :: (Num a) => [a] -> [a]
negateAll' xs = map (negate . abs) xs

-- Note: function composition is right associative

-- Function composition with multiple parameters

-- If we want to use composition with function that takes several parameters
-- we usually must partially apply them so that each function takes only one
-- parameter

-- sum (replicate 5 (max 6.7 8.9))
-- <=>
-- (sum .replicate 5) max 6.7 8.9
-- <=>
-- sum . replicate 5 $ max 6.7 8.9

-- Point-free style
-- Another common use of function composition is defining function in the
-- point-free style.

-- sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (+) 0 xs

-- The xs is on the far right on the both sides of the equal sign. Because of
-- currying we can omit the xs on both sides, since calling foldl (+) 0 creates
-- a function that takes a list. Doing so, we are writing the function in
-- point-free style

-- sum' :: (Num a) => [a] -> a
-- sum' = foldl (+) 0

-- fn x = ceiling (negate (tan (cos (max 50 x))))
-- Here we can't just get rid of the x on both sides since the x in the function
-- body is surrounded by (). cos (max 50) does not make sense, we can't get the
-- cos of a function.
-- fn = ceiling . negate . tan . cos . max 50

-- Better readability, but when the function is too complex it is discouraged
-- doing so. For big functions, the preferred style is to use `let`bindings
-- to give labels to intermediary results or to split the pb into sub pbs.

-- Another version of oddSquareSum
oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<1000) . filter odd $ map (^2) [1..] 
