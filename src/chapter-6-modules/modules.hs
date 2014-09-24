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