-- Factorial function
fat:: Int->Int
fat n | n == 0  = 1
      | n > 0   = n * fat(n-1)

-- Function to compare 3 numbers
allEqual :: Int -> Int -> Int -> Bool
allEqual a b c = (a == b) && (b == c)

-- Function to compare 4 numbers
all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d = (a == b) && (b == c) && (c == d)

-- Function to compare 4 numbers using the allEqual function
all4EqualRec :: Int -> Int -> Int -> Int -> Bool
all4EqualRec a b c d = (allEqual a b c) && (a == d)

-- Function to count how many numbers are equal
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c | allEqual a b c = 3
                   | ((a == b) && (b /= c)) || ((a == c) && (c /= b)) || ((b == c) && (c /= a)) = 2
                   | otherwise = 1