{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model where -- (Punto (..), Semiretta (..), Angolo , TreePath, Tree, Accelerazione, configura, film) where

import Data.Tree (Tree(..))
import Data.Tree.Missing ( zipTreeWith, recurseTreeAccum,  ispettore , Ispettore, moveTop)
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

data Pezzo a = Pezzo Punto Punto deriving (Show,Read)


isometrica  :: Pezzo a -> (Punto, Angolo)
isometrica (Pezzo c o) = (o, atan2 y x) where
    Punto (x,y) = o - c


assolutizza :: Tree (Pezzo Relativo) -> Tree (Pezzo Assoluto)
assolutizza = recurseTreeAccum (Punto (0,0)) f    where
    f q (Pezzo c o) = (qc, Pezzo qc (o + q)) where qc = q + c

relativizza :: Tree (Pezzo Assoluto) -> Tree (Pezzo Relativo)
relativizza = recurseTreeAccum (Punto (0,0)) f    where
    f q (Pezzo c o) = (c, Pezzo qc (o - q)) where qc = c - q

-- prepara le ispezioni del pezzo nell'albero più vicino al punto dato
vicino :: Punto -> Tree (Pezzo Assoluto) -> Ispettore
vicino x tr = ispettore ch tr where
    x' = minimumBy (comparing $ modulus . abs . subtract x) . toList . fmap (fst . isometrica) $ tr
    ch (Pezzo _ o) = o == x'

-- ruota il solo pezzo specificato dall'ispettore
ruotaScelto :: Ispettore -> Angolo -> Tree (Pezzo Relativo) -> Tree (Pezzo Relativo)
ruotaScelto m alpha tr = aggiorna . (\t -> fst (m  t) (\(_,p) -> (alpha,p))) . fmap ((,) 0) $ tr

-- ruota tutti i pezzi dell'angolo assegnato
aggiorna :: Tree (Angolo, Pezzo Relativo) -> Tree (Pezzo Relativo)
aggiorna = recurseTreeAccum id ruotaPezzo where
    ruotaPezzo :: Ruota -> (Angolo, Pezzo Relativo) -> (Ruota, Pezzo Relativo)
    ruotaPezzo r (alpha, Pezzo c o) = let r' = ruota alpha in (r', Pezzo (r c) (r' o))

newtype Tempo a = Tempo {tempo :: Float} deriving (Eq, Show, Read, Num, Fractional, Floating)

data Normalizzato

interpolazione      :: Tree (Pezzo Relativo)
                    -> Tree (Pezzo Relativo)
                    -> Tempo Normalizzato
                    -> Tree (Pezzo Relativo)

interpolazione t1 t2 t = aggiorna $ zipTreeWith variazioneAngolo  t1 t2 where
    variazioneAngolo p p' = ((f p' - f p) /  tempo t, p)  where f = snd . isometrica


spostaFulcro :: Punto ->Punto -> Tree (Pezzo Assoluto) -> Maybe (Tree (Pezzo  Assoluto))
spostaFulcro  n p = moveTop k (\(Pezzo c _ ) -> c == n) (Pezzo p undefined) where
    k (Pezzo c _) (Pezzo c' o') = Pezzo c o'






