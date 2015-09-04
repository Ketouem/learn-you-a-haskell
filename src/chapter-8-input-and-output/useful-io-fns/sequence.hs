-- sequence

-- takes a list of I/O actions and returns an I/O action that will perform
-- those actions one after the other. The result that this I/O action yields
-- will be a list of the results of all the I/O actions that were performed

main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs

-- equivalent
-- main = do
--  a <- getLine
--  b <- getLine
--  c <- getLine
--  print [a,b,c]

-- Common pattern with sequence is when we map functions like print or putStrLn
-- over lists. Executing `map print [1,2,3,4]` won't create an I/O action, but a
-- list of I/O actions. If we want to transform that list into an I/O action
-- We must sequence it `sequence $ map print [1,2,3,4]`