type Person = String
type Book = String
type Database = [(Person , Book)]

books :: Database -> Person -> [Book]
books [] _ = []
books (a:as) b | fst a == b = snd a : books as b
               | fst a /= b = books as b


borrowers :: Database -> Book -> [Person]
borrowers [] _ = []
borrowers (a:as) b | snd a == b = fst a : borrowers as b
                   | snd a /= b = borrowers as b

borrowed :: Database -> Book -> Bool
borrowed db b | borrowers db b /= [] = True
              | otherwise = False

numBorrowed :: Database -> Person -> Int
numBorrowed a b = length (books a b)

makeLoan :: Database -> Person -> Book -> Database
makeLoan db p l = (p,l) : db


returnLoan :: Database -> Person -> Book -> Database
returnLoan (a:as) p l | ((p == fst a) && (l == snd a)) = as
                      | otherwise = returnLoan  p l


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
