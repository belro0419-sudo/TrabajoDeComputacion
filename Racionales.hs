-- Iván De León
-- NRO ESTUDIANTE 1

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
    (==) = undefined
    
instance Eq Racional where
    (==) = undefined

instance Ord Signo where
    (<=) = undefined

instance Ord Racional where
    (<=) = undefined

instance Num Racional where
    (+) = undefined
    (*) = undefined
    (-) = undefined  
