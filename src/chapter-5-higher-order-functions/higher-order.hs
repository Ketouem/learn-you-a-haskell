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
