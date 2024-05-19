module ExercíciosP3.Atividade02.Ex4 where

meuReplicate :: Int -> a -> [a]
meuReplicate a b = [ n | n <- take a (repeat b)]