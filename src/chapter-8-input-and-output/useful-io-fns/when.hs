-- when (Control.Monad)

-- takes a Bool and an I/O action. If that Bool value is true, it returns the same
-- I/O action that we supplied to it. However, if it's False, it returns the
-- return () action, which doesn't do anything

import Control.Monad

main = do
    input <- getLine
    when (input == "SWORDFISH") $ do
        putStrLn input

-- without when
-- main = do
--    input <- getLine
--    if (input == "SWORDFISH")
--        then putStrLn input
--        else return ()