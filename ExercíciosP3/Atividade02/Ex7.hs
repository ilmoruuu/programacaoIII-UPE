module ExercíciosP3.Atividade02.Ex7 where

versaoInicial :: [(Int, Int)]
versaoInicial = [(x,y) | x <- [1, 2], y <- [3, 4]]

versaoConcat :: [(Int, Int)]
versaoConcat = concat [ [(x, y) | y <- [3, 4]] | x <- [1, 2] ]

main :: IO ()
main = do
    print versaoInicial
    print versaoConcat
    print "Podemos observar que podemos utilizar o 'concat' para unir dois resultados"