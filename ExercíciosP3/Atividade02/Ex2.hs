module ExercíciosP3.Atividade02.Ex2 where

grid :: Int -> Int -> [(Int, Int)]
grid a b = [(x,y) | x <- [0..a] , y <- [0..b]]