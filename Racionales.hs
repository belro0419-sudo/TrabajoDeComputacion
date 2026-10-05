-- Iván De León
-- 330339

-- Belen Rodriguez
-- 373879

{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Racionales where
import Naturales

data Signo where { Pos :: Signo ; Neg :: Signo } deriving Show

data Racional where { Q :: Signo -> (N,N) -> Racional } deriving Show


instance Eq Signo where
    (==) = \s1 s2 -> case s1 of 
        Pos -> case s2 of 
            Pos-> True
            Neg-> False
        Neg-> case s2 of
            Pos -> False
            Neg -> True

instance Eq Racional where
    (==) = \r1 r2 -> case r1 of
        Q s1 (a,b) -> case r2 of
            Q s2 (c,d) -> case (s1 == s2) of
                True -> (a * d) == (b * c)
                False -> case (a == O) of
                    True -> case (c == O) of
                        True -> True
                        False -> False
                    False -> False

instance Ord Signo where
    (<=) = \s1 s2 -> case s1 of
    Pos -> case s2 of
        Pos -> True
        Neg -> False
    Neg -> case s2 of
        Pos -> True
        Neg -> True

instance Ord Racional where
    (<=) = \r1 r2 -> case r1 of 
        Q s1 (a,b)-> case r2 of 
            Q s2 (c,d) -> case (s1==s2) of
                True ->case s1 of
                    Pos->(a*d) <= (b*c)
                    Neg->(b*c) <= (a*d)
                False -> case s1 of
                    Neg -> True
                    Pos -> case (a==O) of
                            True -> case (c==O) of
                                True -> True
                                False -> False
                            False -> False
            

instance Num Racional where
    (+) = \r1 r2 -> case r1 of 
        Q s1 (a,b) -> case r2 of 
            Q s2 (c,d) -> case (s1==s2) of
                True -> Q s1 (a*d + b*c, b*d)
                False -> case (a*d == b*c) of
                    True ->  Q s1 (O,b)
                    False-> case (a*d > b*c) of
                        True -> Q s1 (a*d - b*c, b*d)
                        False -> Q s2 (b*c - a*d, b*d)

    (*) = \r1 r2 -> case r1 of 
        Q s1 (a,b) -> case r2 of
            Q s2 (c,d) -> case (a==O) of
            True-> Q s1 (O,b*d)
            False-> case (c==O) of
                True -> Q s1 (O,b*d)
                False -> case (s1 == s2) of
                    True -> Q Pos (a*c,b*d)
                    False -> Q Neg (a*c,b*d)
                     
    (-) =  \r1 r2 -> case r1 of
    Q s1 (a,b) -> case r2 of
        Q s2 (c,d) -> case s2 of
            Pos -> r1 + Q Neg (c,d)
            Neg -> r1 + Q Pos (c,d)