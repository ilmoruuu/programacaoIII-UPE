module ExercíciosP3.AvaliacaoI.AvaliacaoUm where

-- Questao 1
reverso :: [a] -> [a]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]


-- Questao 2
converter :: Int -> String
converter n = show horas ++ ":" ++ show minutos ++ ":" ++ show segundos
              where
                horas = div n 3600
                restoHoras = mod n 3600
                minutos = div restoHoras 60
                segundos = mod restoHoras 60

-- Questao 3
rotateLeft :: Int -> [a] -> [a]
rotateLeft 0 l = l
rotateLeft _ [] = []
rotateLeft n (x:xs) = rotateLeft (n - 1) (xs ++ [x])

-- Questao 4
removeMin :: [Int] -> [Int]
removeMin [] = []
removeMin (x:xs) 
  | x == minimum (x:xs) = xs
  | otherwise = x : removeMin xs


remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove m (x:xs) = if m == x then xs else x : remove m xs


-- Questao 5
escolheFuncoes :: [a -> Bool] -> a -> [a -> Bool]
escolheFuncoes [] _ = []
escolheFuncoes fs v = [f | f <- fs, not (f v)]

mapF :: [t -> a] -> t -> [a]
mapF [] _ = []
mapF fs v = [f v | f <- fs]