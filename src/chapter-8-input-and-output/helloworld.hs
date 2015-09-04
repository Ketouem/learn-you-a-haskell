main = putStrLn "hello, world"

-- > :t putStrLn
-- putStrLn :: String -> IO ()
-- > :t putStrLn "hello, world"
-- putStrLn "hello, world" :: IO ()

-- putStrLn takes a string and returns an I/O action that has a result
-- type of () (empty tuple, known as unit)

-- I/O action is something that when performed will carry out an action
-- with a side effect and will also present some result. That I/O action
-- "yields" this result. Printing a string to the screen doesn't really
-- have any kind of meaningful ret value, so a dummy () is used.

-- An I/O action will be performed when we give it a name of main and
-- then run the program.