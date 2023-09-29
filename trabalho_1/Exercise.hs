{-
 NOME: Sávio Henrique Chaves Mendes
 MATRICULA: 211060693
 
** Intruções: **
* Favor preencher nome e matricula acima
* NÃO importe nenhuma biblioteca EXCETO se na descrição do exercício estiver explícito.
-}

module Root.Exercicios.Exercicios where

-- 1) (Valor da questão: 1,0 ponto) 
-- Defina uma função que retorne o maior entre quatro inteiros.
maior4 :: Int -> Int -> Int -> Int -> Int
maior4 a b c d | ((a >= b) && (a >= c) && (a >= d)) = a
               | ((b >= a) && (b >= c) && (b >= d)) = b
               | ((c >= a) && (c >= b) && (c >= d)) = c
               | otherwise = d

-- 2) (Valor da questão: 1,0 ponto) 
-- Defina uma função que receba uma nota e retorne a menção do aluno.
-- Não se preocupe com a validação do input. A nota sempre será um Número entre 0.0 (inclusive) e 10.0 (inclusive).
-- Considere a seguinte tabela para tradução da menção:
-- De 9 a 10 -> "SS"
-- De 7 a 8.9 -> "MS"
-- De 5 a 6.9 -> "MM"
-- De 3 a 4.9 -> "MI"
-- De 0.1 a 2.9 -> "II"
-- De 0 -> "SR"
converterNotaParaMencao :: Float -> String
converterNotaParaMencao n | ((n >= 9) && (n <= 10)) = "SS"
                          | ((n >= 7) && (n <= 8.9)) = "MS"
                          | ((n >= 5) && (n <= 6.9)) = "MM"
                          | ((n >= 3) && (n <= 4.9)) = "MI"
                          | ((n >= 0.1) && (n <= 2.9)) = "II"
                          | (n == 0) = "SR"

-- 3) (Valor da questão: 1,0 ponto) 
-- defina uma função que retorna um booleano indicando se uma lista de inteiros é decrescente ou não:
isDecrescente :: [Int] -> Bool
isDecrescente [] = True
isDecrescente (b:(a:as)) = a < b && isDecrescente (a:as) 
isDecrescente a = if (length a == 1) then True else isDecrescente a  

-- 4) (Valor da questão: 2,0 pontos) 
-- defina uma função que recebe uma lista de strings como entrada e computa uma lista de pares 
-- de (String, Int) representando o histograma de seus elementos:
inList :: [(String, Int)] -> String -> Bool
inList [] _ = False
inList ls s1 = if (length ([ (x,y) | (x,y) <- ls, x==s1 ]) /= 0) then True else False


add1 :: [(String,Int)] -> String -> [(String,Int)]
add1 [] s = [(s,1)]
add1 l1 s | inList l1 s == True = [if (x == s) then (x,y+1) else (x,y)| (x,y) <- l1]
          | inList l1 s == False = (s,1) : l1


histograma :: [String] -> [(String, Int)]
histograma [] = []
histograma (a:as) = add1 (histograma as) a


-- 5)(Valor da questão: 1,5 ponto) 
-- Defina a função myZipWith que tem como parâmetros uma função binária (que tem dois parâmetros) e duas listas, 
-- retornando uma lista de valores resultantes da aplicação dessa função nos elementos correspondentes dessas 
-- listas:
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (a:as) (b:bs) = [f a b] ++ (myZipWith f as bs)


-- 6) (Valor da questão: 2,0 ponto) 
-- Resolva em Haskell o seguinte problema: a partir de duas notas das provas de cada aluno,
-- determinar a lista dos alunos aprovados, com suas respectivas médias. O resultado deve estar
-- ordenado crescentemente pela média aritmética das notas. A aprovação ocorre se, e somente se, tal
-- média é maior ou igual a cinco.
-- OBSERVAÇÃO: especificamente para este exercício, você pode importar as funções de ordenaçao de listas (como 'sort' ou 'sortBy') se achar necessário.
quicksort :: [(String,Float)] -> [(String,Float)]
quicksort [] = []
quicksort ((p,m):xs) = quicksort [(x,y) | (x,y) <- xs, y <= m] ++ [(p,m)] ++ quicksort [(x,y) | (x,y) <- xs, y > m]


aprovadosOrdemDeMedia :: [(String,Float,Float)] -> [(String,Float)]
aprovadosOrdemDeMedia [] = []
aprovadosOrdemDeMedia ((x,p1,p2):as) | m p1 p2 >= 5 = quicksort((x,m p1 p2) : aprovadosOrdemDeMedia as)
                                     | otherwise = []
                                        where m p b = (p+b)/2  

-- 7) (Valor da questão: 1,5 ponto, sendo 0.5 ponto para cada letra) 
-- Considere a representação de matrizes como lista de listas em que cada elemento da lista é uma lista 
-- que representa uma linha da matriz. Com base nisso, determine as seguintes funções:
--  a) some duas matrizes
--  b) compute a transposta de duas matrizes 
--  c) compute a multiplicação de duas matrizes
-- OBSERVAÇÃO: considere que os inputs são válidos (ou seja, as matrizes são válidas e as suas dimensões são compatíveis para soma e multiplicação)
somaMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
somaMatricial [] _ = []
somaMatricial _ [] = []
somaMatricial l1 l2 = [myZipWith (+) n1 n2 | (n1,n2) <- zip' l1 l2] 
                    where 
                        zip' [] _ = []
                        zip' _ [] = []
                        zip' (a:as) (b:bs) = [(a,b)] ++ zip' as bs


matrizTransposta :: (Eq u,Num u) => [[u]] -> [[u]]
matrizTransposta [] = []
matrizTransposta l1@(x:_) | x /= [] = useHead l1 : matrizTransposta (takeHead l1)
                          | otherwise = []
                            where 
                                takeHead [] = []
                                takeHead (x:xs) = [y | y <- x, head x /= y] : takeHead xs
                                useHead [] = []
                                useHead (a:as) = head a : useHead as


multiplicacaoMatricial :: (Eq u,Num u)  => [[u]] -> [[u]] -> [[u]]
multiplicacaoMatricial [] _ = []
multiplicacaoMatricial _ [] = [] 
multiplicacaoMatricial l1 l2 = [ getMult x (matrizTransposta l2)  | x <- l1]
                                where 
                                    getMult [] _ = []
                                    getMult _ [] = []
                                    getMult l1 l2 = [sum (myZipWith (*) l1 a) | a <- l2 ] 
                                    sum [] = 0
                                    sum (a:as) = a + sum as