-- print
-- takes a value of any type that's an instance of Show (can be repr
-- as string), applies show to that value and ouputs the resulting
-- string to the terminal. ~ putStrLn . show

main = do
    print True
    print 2
    print [3,4,3]