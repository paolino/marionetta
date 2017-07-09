{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}
module Math where

import Data.VectorSpace

-- | un punto nel piano 2d ascissa e ordinata o anche un vettore
newtype Punto = Punto (Float,Float) deriving (Eq,Show, Read, AdditiveGroup)

-- | un angolo
type Angolo = Float

-- campo dei Punto
instance Num Punto where
    (+) (Punto (x,y)) (Punto (x1,y1)) = Punto (x+x1,y+y1)
    negate (Punto (x,y)) = Punto (negate x,negate y)
    (*) = error "Punto Num method undefined used"
    abs x = error $ "abs :" ++ show x ++  " Punto Num method undefined used"
    signum = error "signum : Punto Num method undefined used"
    fromInteger x = error $ "fromInteger " ++ show x ++ ": Punto Num method undefined used"


instance VectorSpace Punto where
    type Scalar Punto = Float
    t *^ (Punto (x,y)) = Punto (x * t,y * t)


type Ruota = Punto -> Punto

-- rotazione intorno all'origine
ruota :: Angolo -> Ruota
ruota alpha (Punto (x,y))= Punto (cos alpha * x - sin alpha * y, sin alpha * x + cos alpha * y)


-- modulo di un vettore
modulus :: Punto -> Float
modulus (Punto (x,y)) = sqrt (x ^ 2 + y ^ 2)


pointOfOnlyRotation :: (Punto,Angolo) -> (Punto,Angolo) -> Punto
pointOfOnlyRotation (p1,alpha1) (p2,alpha2) = let 
    dp@(Punto (dpx,dpy)) = p2 - p1
    s = modulus (p2 - p1)
    x = Punto (s/2 , 0)
    y = Punto (0, s / 2 / tan (alpha / 2))
    alpha = alpha2 - alpha1
    beta = atan2 dpy dpx
    in ruota beta (y  - x) + p2 

 
