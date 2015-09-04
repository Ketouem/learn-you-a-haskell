-- When using do syntax to glue together I/O actions, we can use let syntax
-- to bind pure values to names. Whereas <- is used to perform I/O actions
-- and bind their results to names, let is used when we just want to give
-- names to normal values inside I/O actions.

import Data.Char

main = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $ "Hey " ++ bigFirstName ++ " "
                    ++ bigLastName
                    ++ ", how are you?"

-- <- is for performing I/O actions and binding their results to names.
-- use let bindings to bind pure expression to names. 