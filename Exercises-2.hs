-- Function to make a string with x blank spaces
makeSpaces :: Int -> String
makeSpaces x | x == 0 = ""
             | x > 0  = " " ++ (makeSpaces (x-1))

-- Function to push a string to the right by a spaces
pushRight :: Int -> String -> String
pushRight a b | a == 0 = b
              | a > 0  = (makeSpaces a) ++ b