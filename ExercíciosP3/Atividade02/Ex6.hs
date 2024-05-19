module ExercíciosP3.Atividade02.Ex6 where

fatores :: Int -> [Int]
fatores m = [y | y <- [1..m-1], m `mod` y == 0]

perfeitos :: Int -> [Int]
perfeitos n = [x | x <- [1..n], sum(fatores x) == x]
