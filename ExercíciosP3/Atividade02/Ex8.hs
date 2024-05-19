module ExercíciosP3.Atividade02.Ex8 where

posicoes :: Eq a => a -> [a] -> [Int]
posicoes x xs = [i | (z, i) <- zip xs [0 ..], x == z]

buscar :: Eq a => a -> [(a,b)] -> [b]
buscar k xs = [v | (k', v) <- xs, k == k']

meuPosicoes :: Eq a => a -> [a] -> [Int]
meuPosicoes x xs = buscar x (zip xs [0 ..])