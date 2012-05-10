
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model (Punto (..), Angolo, Pezzo (..), rotazioneInOrigine, routingPezzi
	, ruotaScelto, vicino, Figura , relativizza, assolutizza, Assoluto, Relativo, Normalizzato,  Tempo (..) , interpolazione) where

import Prelude hiding (zipWith)
import Data.VectorSpace ((*^))
import Data.Tree (Tree(..))
import Data.Tree.Missing ( recurseTreeAccum, Routing, modifyTop, inspectTop)
import Control.Applicative ((<$>))
import Control.Monad (ap)
import Data.Foldable (minimumBy, toList)
import Data.List.Zipper
import Data.Ord (comparing)
import Control.Arrow (Arrow(..))
import Math
import Data.Zip
import Debug.Trace

data Relativo
data Assoluto

data Pezzo a = Pezzo
    {    fulcroPezzo :: Punto
    ,    originePezzo ::  Punto
    ,    rotazionePezzo :: Angolo
    } deriving (Show,Read,Eq)


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

normalizzaAngolo alpha
	| alpha < -pi = normalizzaAngolo $ alpha + 2 * pi
	| alpha > pi = normalizzaAngolo $ alpha - 2 * pi
	| otherwise = alpha
interpolazione      :: Tree (Pezzo Relativo)
                    -> Tree (Pezzo Relativo)
                    -> Tempo Normalizzato
                    -> Tree (Pezzo Relativo)
interpolazione t1 t2 t = modifyTop (\(Pezzo _ r alpha) -> Pezzo (l t) r alpha) . aggiorna $ zipWith variazioneAngolo  t1 t2 where
    variazioneAngolo p p' = (tempo t * ((normalizzaAngolo $ rotazionePezzo p') - (normalizzaAngolo $ rotazionePezzo p)), p)
    l t = l0 + tempo t *^ (l1 - l0) 
    l0 = fulcroPezzo (inspectTop t1)
    l1 = fulcroPezzo (inspectTop t2)

type Figura = Tree (Pezzo Relativo)


routingPezzi :: Punto -> Routing (Pezzo Assoluto) -> Tree (Pezzo Assoluto) -> Tree (Pezzo Assoluto)
routingPezzi p r = snd . r (Pezzo p undefined undefined) (\(Pezzo c _ _) (Pezzo _ o alpha) -> Pezzo c o alpha)

rotazioneInOrigine :: Tree (Pezzo Assoluto) -> Tree (Pezzo Assoluto)
rotazioneInOrigine = modifyTop $ \(Pezzo _ o alpha) ->  Pezzo o o alpha







