module ExercíciosP3.AvaliacaoRevisaoII.AvaliacaoRevisaoDois where

-- Questao 1
filtraFunc :: Foldable t => [(a1, t a2)] -> [a1]
filtraFunc [] = []
filtraFunc (x:xs) = if length (snd x) >= 2 then fst x : filtraFunc xs else filtraFunc xs

-- Questao 2
prefixos :: [a] -> [[a]]
prefixos [] = error "Lista vazia"
prefixos [x] = [[x]]
prefixos l = reverse $ pega 1 l []


pega :: Int -> [a] -> [[a]] -> [[a]]
pega n lista retorno = if n == length lista then lista_retorno else pega (n+1) lista lista_retorno
    where
        lista_retorno = take n lista : retorno

-- Questao 3
primos :: Integral a => a -> a -> [a]
primos a b = [ x | x <- lista, length (divisores x) == 2]
    where
        lista | a < b = [a..b]
              | otherwise = [b..a]

divisores :: Integral a => a -> [a]
divisores n = [ x | x <- [1..n], n `mod` x == 0]

-- Questao 4
-- Alternativa a)
condTail :: [a] -> [a]
condTail xs = if null xs then [] else tail xs

-- Alternativa b)
guardsTail :: [a] -> [a]
guardsTail xs | null xs = []
              | otherwise = tail xs

-- Alternativa c)
padraoTail :: [a] -> [a]
padraoTail [] = []
padraoTail xs = tail xs

-- Questao 5 
novoMap :: [b -> b] -> [b] -> [b]
novoMap fs xs = map (aplicaFuncao fs) xs

aplicaFuncao :: [t -> t] -> t -> t
aplicaFuncao [] x = x
aplicaFuncao (f:fs) x = aplicaFuncao fs (f x)

