-- Use 'where' to store an intermediate calculation, names inside the where are visible
-- across all guards
bmiTellWithWhere :: Double -> Double -> String
bmiTellWithWhere weight height
	| bmi <= skinny = "You're underweight, you emo, you!"
	| bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
	| bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
	| otherwise = "You're a whale, congratulations!"
	where 
		bmi = weight / height^2
		skinny = 18.5
-- Warning: where variables are not shared across function of different patterns

-- Pattern matching with where
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
	where
		(f:_) = firstname
		(l:_) = lastname

-- Functions inside where
calcBmis :: [(Double, Double)] -> [Double] 
calcBmis xs = [bmi w h | (w, h) <- xs]
	where
		bmi weight height = weight / height^2

-- Let expressions act like where, they are just way more local as they don't span
-- across guards. pt +: they ca be used anywhere inside the function.
-- Syntax: let <bindings> in <expressions>
cylinder :: Double -> Double -> Double
cylinder r h = 
	let 
		sideArea = 2 * pi * r * h
		topArea = pi * r ^ 2
	in 
		sideArea + 2 * topArea
-- Other pts + : .can be used to introduce functions in local scope
--               .can be separated with ; helpful when you want to bind several variables inline and can't align
--                them in columns
--				 .pattern matching with let expression, useful for quick dismantlement of a tuple and binding those
--                components to names
--				 .can be used inside list comprehension

-- let inside list comprehensions
calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcBmisFat :: [(Double, Double)] -> [(Double)]
calcBmisFat xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

-- case, like in any procedural languages. They are a way to use pattern matching almost anywhere in the code.
-- Syntax: case expression of pattern -> result
--                            pattern -> reuslt
head1 :: [a] -> a
head1 [] = error "No head for empty lists!"
head1 (x:_) = x
-- Interchangeable with
head2 :: [a] -> a
head2 xs = case xs of 
	[] -> error "No head for empty lists!"
	(x:_) -> x
-- Whereas pattern matching on function parameters can be done only when defining functions, case expressions
-- can be used anywhere, even in the middle of an expression
describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of 
	[] 	-> "empty."
	[x] -> "a singleton list."
	xs 	-> "a longer list."
-- alt version with where expressions
describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
	where 
		what [] = "empty."
		what [x] = "a singleton list."
		what xs = "a longer list."