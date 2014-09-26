-- Modules
-- A module is essentially a file that defines some functions, types and type
-- classes.
-- A Haskell program is a collection of modules.

-- A module can have many functions and types defined inside it, and it exports
-- some of them.

-- In the std lib, the `Prelude` module is imported by default

-- Importing Modules

-- Syntax: `import ModuleName`
-- The import must be done before defining any functions. We can import several
-- modules, each import statement need to be on a separate line.

-- Ex.: `Data.List` module for manipulating...lists
import Data.List
import Data.Char

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
-- When importing Data.List, all functions are available
-- nub is one of them: takes a list and filters out duplicate elements
-- the composing : length . nub <=> \xs -> length (nub xs)

-- Note: to search for functions -> http://www.haskell.org/hoogle

-- To import a module inside GHCi : `:m + Data.List Data.Map Data.Set`
-- if the script loaded already used an import, not necessary to reimport

-- Selective import: `import Data.List (nub, sort)`
-- Import all functions excepting some: `import Data.List hiding (nub)`

-- Another way of avoiding name clashes using `qualified`
-- `import qualified Data.List`
-- Data.Map.filter -> call the filter function of the Data.Map module
-- filter -> call the std lib's filter
-- `import qualified Data.List as M`
-- M.filter

-- A.B -> . is regarded as just referring to the imported function
-- A . B -> . is regarded as function composition

-- Solving Problems with Module Functions

-- * Counting Words
-- Data.List.words -> converts string into list of words (string)
-- Data.List.group -> takes a list and groups adjacent element into sublist
--                    if they are equal
-- Data.List.sort -> we need to sort the list after the split and before the
--                   grouping

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

-- Without function composition, quite ugly.
-- wordNums xs = map (\ws -> (head ws, length ws)) (group (sort (words xs)))

-- * Needle in the Haystack
-- Make a function that checks if a list is wholly contained in a second list
-- Data.list.tails -> apply recursively tail on the item of a list
-- Data.list.isPrefixOf -> takes two lists, and tells us if the second one
--                         starts with the first one
-- Data.list.any -> takes a predicate and list and tells us if any element
--                  satisfy the predicate
isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)
-- isIn <=> Data.list.isInfixOf

-- * Caesar cipher
-- We'll apply it on the whole range of the unicode chars
-- Data.Char.ord -> char to code point
-- Data.Char.chr -> code point to char
encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg
-- a composed version (chr . (+ offset) . ord)
decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

-- On strict left folds

-- foldl can sometimes lead to stack overflow errors.
-- When we use foldl, Haskell doesn't evaluate the actual accumulator on every
-- step. Instead, the evaluation is deferred. It also keeps the old deferred
-- computation in memory.

-- Data.list.foldl' provides a stricter version of foldl where the accumulator
-- is evalutaed at each step.
-- A stricter version of foldl1 exists -> foldl1'

-- Finding some cool numbers

-- Ex: find the first natural number such that the sum of its digits equals 40

-- Data.Char.digitToInt : takes a Char and return an Int
--                        '0' to '9' and 'A' to 'F'

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

-- `show` is use to convert the integer into a string

-- Data.List.find : takes a predicate and a list and returns the first element
--                  in the list that matches the predicate.
--                  find :: (a -> Bool) -> [a] -> Maybe a
--                  Maybe a ~ [a] . Whereas a list can have zero, one, or many
--                  elements, a Maybe a typed value can have either 0 elements
--                  or juste one elements. We use it when we want to represent
--                  possible failure. To make a value that holds nothing, we
--                  just use `Nothing`. Analogous to an empty list. To construct
--                  a value that holds something, we write `Just <value .
--                  If `find` finds an element that satisfies the predicate, it
--                  will return that element wrapped in a `Just`, else it will
--                  return a `Nothing`

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

-- Mapping keys to values

-- Association lists (aka dictionaries)

-- Most obvious way to represent it in Haskell would be a list of pairs.
-- The first value being the key and the second the value.

phoneBook =
    [("betty", "555-2938")
    ,("bonnie", "452-2928")]

-- Lookup for a value given a key
findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs
-- The function takes a key and a list, filters the list so that only matching
-- keys remain, gets the first key/value pair that matches and returns the value

-- Handling missing key
findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k,v):xs)
    | key == k  = Just v
    | otherwise = findKey' key xs

-- folded version
findKey'' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey'' key xs = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing xs

-- Enter Data.Map