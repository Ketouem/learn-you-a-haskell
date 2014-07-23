-- Patterns are evaluated from top to bottom
-- REMINDER: Explicit typing is called 'Type annotation'
lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN !"
-- Always include a fallback match to avoid an exception
lucky x = "Sorry, you're out of luck."

sayMe :: Int -> String
sayMe 1 = "One !"
sayMe 2 = "Two !"
sayMe 3 = "Three !"
sayMe 4 = "Four !"
sayMe 5 = "Five !"
sayMe x = "Not between 1 and 5 !"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Pattern matching with tuples

-- 2D vector addition without pattern matching
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)

-- Now with pattern matching
addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Extract elements from triple, fst and snd extract elements from couples only
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

-- Pattern matching in list comprehension
xs = [(1, 3), (5, 6)]
add = [a + b | (a, b) <- xs]

-- Own implementation of the head function
-- REMINDER: [a] is a 'Type Variable'
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy !"
-- If we want to bind something to several variables, we need to surround it with ()
head' (x:_) = x

-- Class constraint
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
-- REMINDER: ++ operator joins two list into one
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- As-patterns: break up item according to pattern while keeping a ref to the original item
firstLetter :: String -> String
firstLetter "" = "Empty string, whoops !"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards, check conditions on values passed during the pattern matching
bmiTell :: Double -> String
-- Guards are indicated by a pipe followed by boolean expression followed by
-- a body function that is used if the expression is True. If False it goes
-- to the next one. Warning: guards must be indented by one space.
bmiTell bmi 
	| bmi <= 18.5 = "You're underweight, you emo, you!"
	| bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
	| bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
	| otherwise = "You're a whale, congratulations!"

bmiTell' :: Double -> Double -> String
bmiTell' weight height
	| weight / height^2 <= 18.5 = "You're underweight, you emo, you!"
	| weight / height^2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
	| weight / height^2 <= 30.0 = "You're fat! Lose some weight, fatty!"
	| otherwise = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
	| a <= b = b
	| otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
	| a == b = EQ
	| a <= b = LT
	| otherwise = GT