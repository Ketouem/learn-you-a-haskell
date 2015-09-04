-- putChar
-- takes a character and returns an I/O action that will print
-- it to the terminal.

main = do
  putChar 't'
  putChar 'e'
  putChar 'h'

-- putStr can be defined recursively with the help of putChar

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
  putChar x
  putStr' xs