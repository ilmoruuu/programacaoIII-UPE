{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module ExercíciosP3.Atividade01.AtividadeUm where

-- Questao 01 
comparacao :: Eq a => a -> a -> a -> Int
comparacao a b c 
    | a == b && b == c && a == c = 3
    | a == b || b == c || a == c = 2
    | otherwise = 0

-- Questao 02
media :: Float -> Float -> Float -> Float
media x y z = (x + y + z) / 3.0

compararMaior :: Float -> Float -> Float-> Float
compararMaior x y z
    | x > med && y > med && z > med = 3
    | x > med && y > med || x > med && z > med || y > med && z > med = 2
    | x > med || y > med || z > med = 1
    | otherwise = 0
  where
    med = media x y z

-- Questao 03
potencia_2 :: Int -> Int
potencia_2 x = x * x

-- Questao 04
potencia_4 :: Int -> Int
potencia_4 x = potencia_2 (potencia_2 x)

-- Questao 05
ouExclusivo :: Bool -> Bool -> Bool
ouExclusivo a b = (a || b) && not (a && b)

-- Questao 06
delta :: Float -> Float -> Float -> Float
delta a b c = b^2 - 4*a*c

x_maior :: Float -> Float -> Float -> Float
x_maior a b c = (-b + sqrt(delta a b c)) / (2 * a)

x_menor :: Float -> Float -> Float -> Float
x_menor a b c = (-b - sqrt(delta a b c)) / (2 * a)

-- Questao 07
somaComLimites :: Int -> Int -> Int
somaComLimites n1 n2 = sum [n1..n2]

somaSemLimites :: Int -> Int -> Int
somaSemLimites n1 n2 = sum [n1 + 1 .. n2 - 1] 

-- Questao 08
encontrarMultiplos :: Int -> Int -> Int -> [Int]
encontrarMultiplos n1 n2 n3 = filter (\x -> x `mod` n3 == 0) [n1..n2]

-- Questao 09
multiplicacao :: Int -> Int -> Int
multiplicacao n1 n2 = sum (replicate n1 n2)

-- Questao 10 
mod2 :: Int -> Int -> Int
mod2 _ 0 = error "Divisão por zero"
mod2 x y
    | x < y     = x
    | otherwise = mod2 (x - y) y

-- Questao 11
sequencia :: Int -> Double
sequencia 1 = sqrt 6
sequencia n = sqrt (6 + sequencia (n - 1))

-- Questao 12
