-- We use the `do` syntax to glue together several I/O actions
-- into one.

main = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn ("Hey " ++ name ++ ", you rock!")

-- Each of these steps is an I/O action. By putting them together
-- we glued them into one I/O action. The action that we got as type
-- of IO (), type of the last IO action inside.
-- main always has a type signature of main :: IO something where
-- something is a concrete type.

-- > :t getLine
-- getLine :: IO String
-- getLine is an I/O action that yields a String

-- can't do `nameTag = "Hello, my name is " ++ getLine`
-- does not work because ++ requires both its parameters to be lists over
-- the same type. Left param: String, right param: IO String

-- Except for the last line, every line in a do block that does not bind
-- can also be written with a bind.