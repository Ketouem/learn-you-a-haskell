main = do
  line <- getLine
  if null line
      then return ()
      else do
          putStrLn $ reverseWords line
          main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- return is sort of the opposite of <- While return takes
-- a value and wraps it up in a box, <- takes a box (and performs it)
-- and take the value out of it, binding it to a name.

-- When dealing with with I/O `do` blocks, we mostly use return either
-- because we need to create an I/O action that does not do anything
-- or because we do not want the I/O action that is made up from a `do`
-- block to have the result value of its last action.