fat:: Int->Int
fat n | n == 0  = 1
      | n > 0   = n * fat(n-1)

allEqual :: Int -> Int -> Int -> Bool
allEqual a b c = (a == b) && (b == c)

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d = (a == b) && (b == c) && (c == d)

all4EqualRec :: Int -> Int -> Int -> Int -> Bool
all4EqualRec a b c d = (allEqual a b c) && (a == d)

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c | allEqual a b c = 3
                   | ((a == b) && (b /= c)) || ((a == c) && (c /= b)) || ((b == c) && (c /= a)) = 2
                   | otherwise = 1

main = putStrLn (show (fat 4) ++ "\n" ++ show (all4Equal 4 4 4 4) ++ "\n" ++ show (all4EqualRec 1 2 3 4) ++ "\n" ++ show (howManyEqual 4 2 4) ++ "\n" ++ show (howManyEqual 2 2 2))