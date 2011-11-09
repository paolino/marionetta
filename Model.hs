
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model where -- (Punto (..), Semiretta (..), Angolo , TreePath, Tree, Accelerazione, configura, film) where

import Prelude hiding (zipWith)
import Data.Tree (Tree(..))
import Data.Tree.Missing ( recurseTreeAccum)
import Control.Applicative ((<$>))
import Control.Monad (ap)
import Data.Foldable (minimumBy, toList)
import Data.List.Zipper
import Data.Ord (comparing)
import Control.Arrow (Arrow(..))
import Debug.Trace
import Data.Zip

-- | un punto nel piano 2d ascissa e ordinata o anche un vettore
newtype Punto = Punto (Float,Float) deriving (Eq,Show, Read)

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


type Ruota = Punto -> Punto

-- rotazione intorno all'origine
ruota :: Angolo -> Ruota
ruota alpha (Punto (x,y))= Punto (cos alpha * x - sin alpha * y, sin alpha * x + cos alpha * y)

-- modulo di un vettore
modulus :: Punto -> Float
modulus (Punto (x,y)) = sqrt (x ^ 2 + y ^ 2)

data Relativo
data Assoluto

data Pezzo a = Pezzo
    {    fulcroPezzo :: Punto
    ,    originePezzo ::  Punto
    ,    rotazionePezzo :: Angolo
    } deriving (Show,Read)





assolutizza :: Tree (Pezzo Relativo) -> Tree (Pezzo Assoluto)
assolutizza = recurseTreeAccum (Punto (0,0)) f    where
    f q (Pezzo c o alpha) = (qc, Pezzo qc (o + qc) alpha ) where qc = q + c


relativizza :: Tree (Pezzo Assoluto) -> Tree (Pezzo Relativo)
relativizza = recurseTreeAccum (Punto (0,0)) f    where
    f q (Pezzo c o alpha) = (c, Pezzo (c - q) (o - c) alpha)

-- prepara le ispezioni del pezzo nell'albero più vicino al punto dato
vicino :: Punto -> Tree (Pezzo Assoluto) -> Selector Tree b
vicino x tr = mkSelector ch tr where
    x' = minimumBy (comparing $ modulus .  subtract x) . toList . fmap originePezzo $ tr
    ch (Pezzo _ o _) = o == x'

-- ruota il solo pezzo specificato dall'ispettore
ruotaScelto :: Selector Tree (Angolo, Pezzo Relativo) -> Angolo -> Tree (Pezzo Relativo) -> Tree (Pezzo Relativo)
ruotaScelto m alpha tr = aggiorna . (\t -> fst (m  t) (\(_,p) -> (alpha,p))) . fmap ((,) 0) $ tr

-- ruota tutti i pezzi dell'angolo assegnato
aggiorna :: Tree (Angolo, Pezzo Relativo) -> Tree (Pezzo Relativo)
aggiorna = recurseTreeAccum id ruotaPezzo

ruotaPezzo :: Ruota -> (Angolo, Pezzo Relativo) -> (Ruota, Pezzo Relativo)
ruotaPezzo r (alpha, Pezzo c o beta) = let r' = ruota alpha in (r', Pezzo (r c) (r' o) $ alpha + beta)

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
interpolazione t1 t2 t = aggiorna $ zipWith variazioneAngolo  t1 t2 where
    variazioneAngolo p p' = ((rotazionePezzo p' - rotazionePezzo p) /  tempo t, p)
type Figura = Tree (Pezzo Relativo)
type Renderer b = Pezzo Assoluto -> b

type Rendering b = Tree (Renderer b)

renderFigura :: Rendering b -> Figura -> [b]
renderFigura r x =  toList . zipWith ($) r . assolutizza $ x









