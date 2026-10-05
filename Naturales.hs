-- Iván De León
-- 330339

-- Belen Rodriguez
-- 373879

data N where
    O :: N
    S :: N -> N

uno :: N
uno = S O
dos :: N
dos = S uno
tres :: N
tres = S dos
cuatro :: N
cuatro = S tres
cinco :: N
cinco = S cuatro
seis :: N
seis = S cinco
siete :: N
siete = S seis
ocho :: N
ocho = S siete
nueve :: N
nueve = S ocho
predecesor :: N -> N
predecesor = \n -> case n of {O -> O; S x -> x}

instance Eq N where{
    (==) = \n1 n2 -> case n1 of 
        O -> case n2 of 
            O -> True;
            S n -> False
        S m -> case n2 of 
            O -> False
            S n -> m == n
}

instance Ord N where {
    (<=)= \ n1 n2 -> case n1 of 
        O -> case n2 of 
            O -> True
            S n -> True
        S m -> case n2 of 
            O -> False 
            S n -> m <= n
}

instance Num N where {
    (+) = \n1 n2 -> case n1 of {
        O -> n2;
        S m -> S (m + n2)
    };

    (*) = \n1 n2 -> case n1 of {
        O -> O;
        S m -> n2 + (m * n2)
    };

    (-) = \n1 n2 -> case n1 of {
        O -> O;
        S m -> case n2 of {
            O -> S m;
            S n -> m - n
        }
    }
}