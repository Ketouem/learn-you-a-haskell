-- forever (Control.Monad)

-- Takes an I/O action and returns an I/O action that just repeat the I/O action
-- it got forever.

import Control.Monad
import Data.Char

main = forever $ do
    putStr "Gimme some input: "
    l <- getLine
    putStrLn $ map toUpper l
