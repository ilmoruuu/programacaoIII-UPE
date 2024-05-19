module ExercíciosP3.Atividade02.Ex5 where

pitag :: Int -> [(Int, Int, Int)]
pitag n = [(x,y,z) | x <- [1..n] , y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]