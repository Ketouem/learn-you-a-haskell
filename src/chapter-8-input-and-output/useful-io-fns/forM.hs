-- forM (Control.Monad)

-- like mapM, but its parameters are switched around. The first parameter is the list, and
-- the second is the function to map over that list, which is then sequenced.

import Control.Monad

main = do
    colors <- forM [1,2,3,4] (\a -> do
      putStrLn $ "Which color do you associate with the number "
                 ++ show a ++ "?"
      color <- getLine
      return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors
