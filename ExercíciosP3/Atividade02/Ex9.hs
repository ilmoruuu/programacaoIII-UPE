module ExercíciosP3.Atividade02.Ex9 where

produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar xs ys = sum [x * y | (x,y) <- zip xs ys]