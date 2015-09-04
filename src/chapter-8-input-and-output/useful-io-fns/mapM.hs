-- mapM & mapM_

-- Because mapping a function that returns an I/O action over a list and then sequencing
-- it is so common, the utility functions mapM and mapM_ were introduced.
-- mapM takes a function and a list, maps the function over the list and sequence it.
-- mapM_ does the same thing but throws away the result later