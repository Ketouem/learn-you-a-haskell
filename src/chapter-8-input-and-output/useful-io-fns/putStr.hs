-- putStr
-- much like puStrLn, takes a string as a param and returns an IO action
-- that will print that string to the terminal. But does not add a new line.

main = do
  putStr "Hey, "
  putStr "I'm "
  putStrLn "Andy!"