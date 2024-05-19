module ExercíciosP3.Atividade02.Ex1 where
    
listaQuadrado :: Int -> Int
listaQuadrado a = sum [a^2 | a <- [1..100]]