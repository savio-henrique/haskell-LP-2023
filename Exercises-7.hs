data Expr = Lit t |
            Add Expr Expr |
            Sub Expr Expr

data List t = Nil |
                Cons t (List t)

data Tree t = Nil |
                Node t (Tree t) (Tree t)

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ "-" ++ showExpr e2 ++ ")"

depth :: Tree t -> Int
depth Nil t = 0
depth (Node _ t1 t2) = 1 + maxi(depth t1,depth t2)

colapse :: Tree t -> [t]
colapse Nil t = []
colapse (Node s t1 t2) = n : ((colapse t1) ++ (colapse t2))

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree _ Nil t = Nil t
mapTree f (Nil s t1 t2) = Node (f s) (mapTree f t1) (mapTree f t2)