module ExercíciosP3.Questions99.Problems0to10 where

-- Problem 01 = "Localizar o último elemento de uma lista."
myLastElement :: [a] -> a
myLastElement [x] = x
myLastElement [] = error "Don't have any element"

myLastElement x = last x

-- Problem 02 = "Encontre o penúltimo elemento de uma lista."
myButLast :: [a] -> a
myButLast [x] = x
myButLast [] = error "Don't have any element"

myButLast x = last (init x)

-- Problem 03 = "Encontre o elemento k-ésimo de uma lista."
elementAt :: [a] -> Int -> a
elementAt list k = list !! (k-1)

-- Problem 04 = "Localize o número de elementos em uma lista."
myLenght :: [a] -> Int
myLenght [] = 0
myLenght x = length x

-- Problem 05 = "Inverter uma lista."
myReverse :: [a] -> [a]
myReverse [] = error "There is no element to reverse"
myReverse x = reverse x

-- Problem 06 = "Descubra se uma lista é um palíndromo."
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = error "There is no element to comparate"
isPalindrome x = x == reverse x

-- Problem 07 = "Nivelar uma estrutura de lista aninhada."
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- Problem 08 = "Elimine duplicatas consecutivas de elementos de lista."
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
    | x == y = compress (y:xs)
    | otherwise = x : compress (y:xs)

-- Problem 09 = "Empacote duplicatas consecutivas de elementos de lista em sublistas."
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x `elem` head (pack xs)
              then (x:head (pack xs)):tail (pack xs)
              else [x]:pack xs

-- Problem 10 = "Codificação de comprimento de execução de uma lista."
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = (length $ x : takeWhile (==x) xs, x)
                 : encode (dropWhile (==x) xs)