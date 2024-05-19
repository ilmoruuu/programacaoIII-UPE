module ExercíciosP3.Atividade02.Ex3 where

grid :: Int -> Int -> [(Int, Int)]
grid a b = [(x,y) | x <- [0..a] , y <- [0..b]]

quadrado :: Int -> [(Int,Int)]
quadrado n = [(x,y) | (x,y) <- grid n n, x /= y]
