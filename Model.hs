
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model where -- (Punto (..), Semiretta (..), Angolo , TreePath, Tree, Accelerazione, configura, film) where

import Data.Tree (Tree(..))
import Data.Tree.Missing ( zipTreeWith, recurseTreeAccum,  ispettore , Ispettore)
import Control.Applicative ((<$>))
import Control.Monad (ap)
import Data.Foldable (minimumBy, toList)
import Data.List.Zipper
import Data.Ord (comparing)
import Control.Arrow (Arrow(..))

-- | un punto nel piano 2d ascissa e ordinata o anche un vettore
newtype Punto = Punto (Float,Float) deriving (Eq,Show, Read)

-- | un angolo
type Angolo = Float

-- campo dei Punto
instance Num Punto where
	(+) (Punto (x,y)) (Punto (x1,y1)) = Punto (x+x1,y+y1)
	negate (Punto (x,y)) = Punto (negate x,negate y)
	(*) = error "Punto Num method undefined used"
	abs = error "Punto Num method undefined used"
	signum = error "Punto Num method undefined used"
	fromInteger = error "Punto Num method undefined used"


type Ruota = Punto -> Punto

-- rotazione intorno all'origine
ruota :: Angolo -> Ruota
ruota alpha (Punto (x,y))= let
    a = alpha * pi / 180
    in Punto (cos a * x - sin a * y, sin a * x + cos a * y)

-- modulo di un vettore
modulus :: Punto -> Float
modulus (Punto (x,y)) = sqrt (x ^ 2 + y ^ 2)

data Relativo
data Assoluto

data Pezzo a = Pezzo
    {    fulcroPezzo :: Punto
    ,    origineOAngolo :: Either Punto Angolo
    } deriving (Show,Read)


polare  :: Pezzo a -> (Punto, Angolo)
polare (Pezzo c (Left o)) = (o, atan2 y x) where
    Punto (x,y) = o - c
polare (Pezzo c (Right alpha)) = (c,alpha)


assolutizza :: Tree (Pezzo Relativo) -> Tree (Pezzo Assoluto)
assolutizza = recurseTreeAccum (Punto (0,0)) f    where
    f q (Pezzo c (Left o)) = (qc, Pezzo qc $ Left (o + q)) where qc = q + c
    f q (Pezzo c alpha) = (qc, Pezzo qc alpha) where qc = q + c

relativizza :: Tree (Pezzo Assoluto) -> Tree (Pezzo Relativo)
relativizza = recurseTreeAccum (Punto (0,0)) f    where
    f q (Pezzo c (Left o)) = (c, Pezzo qc $ Left (o - q)) where qc = c - q
    f q (Pezzo c alpha) = (c, Pezzo (c - q) alpha)

-- prepara le ispezioni del pezzo nell'albero più vicino al punto dato
vicino :: Punto -> Tree (Pezzo Assoluto) -> Ispettore b
vicino x tr = ispettore ch tr where
    x' = minimumBy (comparing $ modulus . abs . subtract x) . toList . fmap (fst . polare) $ tr
    ch (Pezzo _ (Left o)) = o == x'
    ch (Pezzo c alpha) = c == x'

-- ruota il solo pezzo specificato dall'ispettore
ruotaScelto :: Ispettore (Angolo, Pezzo Relativo) -> Angolo -> Tree (Pezzo Relativo) -> Tree (Pezzo Relativo)
ruotaScelto m alpha tr = aggiorna . (\t -> fst (m  t) (\(_,p) -> (alpha,p))) . fmap ((,) 0) $ tr

-- ruota tutti i pezzi dell'angolo assegnato
aggiorna :: Tree (Angolo, Pezzo Relativo) -> Tree (Pezzo Relativo)
aggiorna = recurseTreeAccum id ruotaPezzo

ruotaPezzo :: Ruota -> (Angolo, Pezzo Relativo) -> (Ruota, Pezzo Relativo)
ruotaPezzo r (alpha, Pezzo c (Left o)) = let r' = ruota alpha in (r', Pezzo (r c) $ Left (r' o))
ruotaPezzo r (alpha, Pezzo c (Right beta)) = let r' = ruota alpha in (r', Pezzo (r c) . Right $ beta + alpha)

newtype Tempo a = Tempo {tempo :: Float} deriving (Eq, Show, Read)

data Normalizzato

tf :: (Float -> Float -> Float) -> Tempo a -> Tempo b -> Tempo c
tf g (Tempo x) (Tempo y) = Tempo (x `g` y)

(.+.) :: Tempo Assoluto -> Tempo Relativo -> Tempo Assoluto
(.+.) = tf (+)
(./.) :: Tempo Relativo -> Tempo Relativo -> Tempo Normalizzato
(./.) = tf (/)
(.-.) :: Tempo Assoluto -> Tempo Assoluto -> Tempo Relativo
(.-.) = tf (-)

interpolazione      :: Tree (Pezzo Relativo)
                    -> Tree (Pezzo Relativo)
                    -> Tempo Normalizzato
                    -> Tree (Pezzo Relativo)
interpolazione t1 t2 t = aggiorna $ zipTreeWith variazioneAngolo  t1 t2 where
    variazioneAngolo p p' = ((f p' - f p) /  tempo t, p)  where f = snd . polare








