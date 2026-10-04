-- explicacion : eto es para decirle a haskel q O (lo q vamos a usar como 0)es un natural y q S es un naturarl q nos devuelve otro natural
data N where
    O :: N
    S :: N -> N
--copiamos lo q esta en lab2 para facilitar la vida ypredecesor para poder hacer recursividad
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
predecesor :: N -> N
predecesor = \n -> case n of {O -> O; S x -> x}

--aca lo q hacemos es ver si son igls  ponemos 2 numeros (n1y n2 ) y vemos si n1 es 0 vemos n2 si n2 es 0 tm entonves son igl si es distinto de 0 (S n) es falso,
--dsos si n1 es S m(un numre) vemos q pasa con n2 ,si es 0 es falso y sino se analisa si m es ig a n ,ahi te va a decir automaticamente si es true o false 
instance Eq N where{
    (==) = \n1 n2 -> case n1 of 
        O -> case n2 of 
            O -> True;
            S n -> False
        S m -> case n2 of 
            O -> False
            S n -> m == n
}
--con esta hacemos tnt <= como > prq con tener una ya analiza el resto ,si n1 es 0 analisamos n2 ,esto siemre da verdader prq 0 es <= a 0 y 0 siempe va a ser <= a cualquier cosa
--si n1 es un numero evaluamos n2y 0 nos da falso prq 0 no es menor a un numero x y si n2 es otro num evaluamos y igl q en la anterior nos va a tirar si es trueo falsa
instance Ord N where {
    (<=)= \ n1 n2 -> case n1 of 
        O -> case n2 of 
            O -> True
            S n -> True
        S m -> case n2 of 
            O -> False 
            S n -> m <= n
}
--aca hacemos la instanciacio n de las cuentas es lo mismo q ya hicimos en las otras 2 cosas solos q en - como estamos en naturales y los naturales no tienen negs se deja comom 0
-- en el de + q es el q tiene algo raro,S(M + n2) se hace prq m es un numero sy S(m) es es numero +1 ,entonces S(m+n2) es como 1+(2+3) S(3) y n2=3 -> 1+(2+3) -> 1+5 -> 6
instance Num N where{
    (+) = \ n1 n2 -> case n1 of 
        O -> n2 
        S m -> S(m + n2);
    (*) = \ n1 n2 -> case n1 of 
        O-> O;
        S m -> n2 + (m * n2);
    (-) = \ n1 n2 -> case n1 of 
        O -> n2;
        S m -> case n2 of 
            O-> O ;
            S n -> m - n
}
