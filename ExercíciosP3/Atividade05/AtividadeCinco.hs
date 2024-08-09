module ExercíciosP3.Atividade05.AtividadeCinco where

--Questao 01
data Nat = Zero | Suc Nat deriving Show

somar :: Nat -> Nat -> Nat
somar Zero n = n
somar (Suc m) n = Suc (somar m n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult x (Suc Zero) = x
mult x (Suc y) = somar x (mult x y) 

--Questao 02
