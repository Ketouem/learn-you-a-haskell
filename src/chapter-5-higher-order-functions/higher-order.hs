-- Sections: infix functions can be partially applied by using sections. Surround
-- the infix function with parameter, that creates a function that takes one parameter
-- and then applies it to the side that' missing.
divideByTen :: (Floating a) => a -> a
divideByTen = (/10) --same as (/10) applied on a

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- WARNING: section and negative operator (-4) == -4 (fon convenience)
--          to create a section that substracts for: (substract 4)