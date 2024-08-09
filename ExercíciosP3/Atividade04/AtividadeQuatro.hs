module ExercíciosP3.Atividade04.AtividadeQuatro where

-- Questao 01
impares :: [Int] -> [Int]
impares (x:xs) = filter odd (x:xs)

-- Questao 02
posicao :: Int -> [a] -> a
posicao 0 (x:xs) = x
posicao p (x:xs)
    | p < 0 = error "Não é possível acessar posições negativas"
    | p > 0 = posicao (p - 1) xs

-- Questao 03 *
repete :: Int -> [[Int]]
repete 1 = [[1]]
repete x
    | x > 1 = replicate x x : repete (x - 1)
    | x < 0 = error "Não são aceitos números negativos"

-- Questao 04 
palindromo :: Eq a => [a] -> Bool
palindromo [] = True
palindromo xs = xs == reverse xs

-- Questao 05
fibonacci :: Int -> [Int]
fibonacci n
    | n < 0 = error "Não existe elementos da sequência de Fibonacci negativos"
    | n == 0 = [0]
    | n == 1 = [0, 1]
    | otherwise = fib 0 1 n
    where
        fib a b 1 = [a]
        fib a b 2 = [a, b]
        fib a b x = a : fib b (a + b) (x - 1)

-- Questao 06
-- Letra A