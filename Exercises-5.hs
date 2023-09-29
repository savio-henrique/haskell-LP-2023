type Person = String
type Book = String
type Database = [(Person , Book)]

member :: [Int] -> Int -> Bool
member [] _ = False
member ls n = length [xs | xs <- ls, xs == n] > 0

books :: Database -> Person -> [Book]
books [] _ = []
books ls p = [ y | (x,y) <- ls, x == p]

borrowers :: Database -> Book ->[Person]
borrowers ls b = [x | (x,y) <- ls, y == b]

borrowed :: Database -> Book -> Bool
borrowed ls b = length (borrowers ls b) > 0

returnLoan :: Database -> Person -> Book -> Database
returnLoan [] _ _ = []
returnLoan ls p b = [(ps,bs) | (ps,bs) <- ls, ps /= p || bs /= b]

s :: Person
s = "Savio"

c :: Person
c = "Carlos"

l1 :: Book
l1 = "Livro 1"

l2 :: Book
l2 = "Livro 2"

l3 :: Book
l3 = "Livro 3"

banco :: Database
banco = [(s,l1),(s,l2),(c,l3),(c,l2)]