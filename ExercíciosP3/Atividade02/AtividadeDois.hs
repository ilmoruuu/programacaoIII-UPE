module ExercíciosP3.Atividade02.AtividadeDois where

-- Questao 01
listaQuadrado :: Int -> Int
listaQuadrado a = sum [a^2 | a <- [1..100]]

-- Questao 02
grid :: Int -> Int -> [(Int, Int)]
grid a b = [(x,y) | x <- [0..a] , y <- [0..b]]

-- Questao 03
quadrado :: Int -> [(Int,Int)]
quadrado n = [(x,y) | (x,y) <- grid n n, x /= y]

-- Questao 04
meuReplicate :: Int -> a -> [a]
meuReplicate a b = [ n | n <- take a (repeat b)]

-- Questao 05
pitag :: Int -> [(Int, Int, Int)]
pitag n = [(x,y,z) | x <- [1..n] , y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- Questao 06
fatores :: Int -> [Int]
fatores m = [y | y <- [1..m-1], m `mod` y == 0]

perfeitos :: Int -> [Int]
perfeitos n = [x | x <- [1..n], sum(fatores x) == x]

-- Questao 07
versaoInicial :: [(Int, Int)]
versaoInicial = [(x,y) | x <- [1, 2], y <- [3, 4]]

versaoConcat :: [(Int, Int)]
versaoConcat = concat [ [(x, y) | y <- [3, 4]] | x <- [1, 2] ]

main :: IO ()
main = do
    print versaoInicial
    print versaoConcat
    print "Podemos observar que podemos utilizar o 'concat' para unir dois resultados"

-- Questao 08      
posicoes :: Eq a => a -> [a] -> [Int]
posicoes x xs = [i | (z, i) <- zip xs [0 ..], x == z]

buscar :: Eq a => a -> [(a,b)] -> [b]
buscar k xs = [v | (k', v) <- xs, k == k']

meuPosicoes :: Eq a => a -> [a] -> [Int]
meuPosicoes x xs = buscar x (zip xs [0 ..])

-- Questao 09
produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar xs ys = sum [x * y | (x,y) <- zip xs ys]