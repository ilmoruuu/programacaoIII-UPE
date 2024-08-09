{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module ExercíciosP3.Atividade03.AtividadeTres where
import Prelude hiding ((^))

-- Questao 01
meuFatorial :: Int -> Int
meuFatorial 0 = 1
meuFatorial n
    | n < 0 = error "Não são aceitos valores negativos"
    | n > 0 = n * meuFatorial (n - 1)

-- Questao 02
minhaSoma :: Int -> Int
minhaSoma 0 = 0
minhaSoma x
    | x < 0 = error "Não são aceitos valores negativos"
    | x > 0 = x + minhaSoma (x - 1)

-- Questao 03
(^^^) :: Num a => a -> a -> a
m ^^^ _ = 1
m ^^^ n = m * (m  ^^^ (n-1))

-- Questao  04
euclides :: Int -> Int -> Int
euclides x xs
    | mod x xs == 0 = xs
    | mod xs x == 0 = x
    | x > xs = euclides xs (mod x xs)
    | x < xs = euclides x (mod xs x)

-- Questao 05
-- Letra A
meuAnd :: [Bool] -> Bool
meuAnd [] = True
meuAnd (x:xs) = x && meuAnd xs

-- Letra B
meuConcat :: [[a]] -> [a]
meuConcat [] = []
meuConcat (x:xs) = x ++ meuConcat xs

--Letra C
meuReplicateDois :: Int -> a -> [a]
meuReplicateDois x y
    | x <= 0 = []
    | otherwise = y : meuReplicateDois (x-1) y

-- Letra D
(!!!!) :: [a] -> Int -> a
(!!!!) (x:xs) 0 = x
(!!!!) (x:xs) y
    | y > length (x:xs) || y < 0 = error "Posição não encontrada"
    | otherwise = xs !!!! (y - 1)

-- Letra E
meuElem :: Eq a => a -> [a] -> Bool
meuElem _ [] = False
meuElem y (x:xs)
    | y == x    = True
    | otherwise = meuElem y xs

-- Questao 06
meuMerge :: Ord a => [a] -> [a] -> [a]
meuMerge xs [] = xs
meuMerge [] ys = ys
meuMerge (x:xs) (y:ys)
    | x <= y    = x : meuMerge xs (y:ys) 
    | otherwise = y : meuMerge (x:xs) ys

-- Questao 07 

-- Questao 08
-- Letra A
somaInt :: [Int] -> Int
somaInt [] = 0
somaInt (x:xs) = x + somaInt xs

-- Letra B
numElem :: [a] -> Int
numElem [] = 0
numElem (x:xs) = 1 + numElem xs

-- Letra C
ultElem :: [a] -> a
ultElem [x] = x
ultElem (_:xs) = ultElem xs
