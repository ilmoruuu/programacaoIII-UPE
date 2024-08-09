module ExercíciosP3.AulaVinteSeteDoSeis where
data Nat = Zero | Suc Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Suc n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Suc (int2nat (n-1))

somar :: Nat -> Nat -> Nat
somar Zero n = n
somar (Suc m) n = Suc (somar m n)

subtracao ::  Nat -> Nat -> Nat
subtracao x y
    | nat2int y > nat2int x = Zero
    | otherwise = int2nat (nat2int x - nat2int y)

subtracaoCorreta :: Nat -> Nat -> Nat
subtracaoCorreta Zero n = Zero
subtracaoCorreta m Zero = m
subtracaoCorreta (Suc m) (Suc n) =  subtracaoCorreta m n