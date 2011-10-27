module Model where -- (Punto (..), Semiretta (..), Angolo , TreePath, Tree, Accelerazione, configura, film) where

import Data.Tree (Tree)
import Data.Tree.Missing (replaceTreeNode, zipTreeWith, recurseTreeAccum, TreePath, modifyNode, pick)
import Control.Applicative ((<$>))
import Data.Foldable (toList)


-- | un punto nel piano 2d ascissa e ordinata
newtype Punto = Punto (Float,Float) deriving (Eq,Show, Read)

-- | un angolo
type Angolo = Float

instance Num Punto where
	(+) (Punto (x,y)) (Punto (x1,y1)) = Punto (x+x1,y+y1)
	negate (Punto (x,y)) = Punto (negate x,negate y)
	(*) = error "Punto Num method undefined used"
	abs = error "Punto Num method undefined used"
	signum = error "Punto Num method undefined used"
	fromInteger = error "Punto Num method undefined used"

-- Calcola le coordinate del punto ruotato di un angolo rispetto ad un punto data
ruota :: Punto -> Angolo -> Punto -> Punto
ruota q x p = let
	Punto (a,o)  = p - q
	z = x*pi/180 + atan2 o a
	r = sqrt (a ^ 2 + o ^ 2)
        in q + Punto (r * cos z, r * sin z)

-- | Astrazione minima per un elemento solido in 2 dimensioni
data Semiretta = Semiretta Punto Angolo deriving (Show,Read, Eq)

type Tempo = Float

type Spostamento = Punto -> Punto

spostamento :: Spostamento -> Semiretta -> Semiretta
spostamento f (Semiretta p t) = Semiretta (f p) t

rotazione :: Angolo -> Semiretta -> (Semiretta, Spostamento)
rotazione r (Semiretta p t) = (Semiretta p r, ruota p (r - t))


spiazzamento :: Semiretta -> Semiretta -> Spostamento
spiazzamento (Semiretta q0 t) (Semiretta q1 r) p = p + (q1 - q0)

data Movimento = Movimento Spostamento Spostamento

-- | core logic 
movimento :: Movimento -> (Angolo ,Semiretta) -> (Movimento, Semiretta)
movimento  (Movimento rot tras) (r,s)= let
   (s', rot') = rotazione r . spostamento rot . spostamento tras $ s
   in ( Movimento rot' (spiazzamento s s'), s')

data Interpolazione = Interpolazione (Tempo -> Spostamento) (Tree (Tempo -> Angolo))

mkInterpolazione :: Tree Semiretta -> Tree Semiretta -> Interpolazione
mkInterpolazione t1 t2  = Interpolazione (\t -> \q -> q + t `scale` (p - q)) (zipTreeWith f t1 t2) where
	f (Semiretta _ x) (Semiretta _ y) t = x + t * (y - x)
	Semiretta p _ = maybe (error "albero vuoto") id $ pick [] t1
	Semiretta q _ = maybe (error "albero vuoto") id $ pick [] t2
	scale t (Punto (x,y)) = Punto (t * x, t * y)

interpola :: Interpolazione -> Tree Semiretta -> Tempo -> Tree Semiretta
interpola (Interpolazione tras trots) ts t = recurseTreeAccum (Movimento (tras t) id) movimento ts' where
	ts' = zipTreeWith (\ta s -> (ta t, s)) trots ts


sigmoide :: Float -> Float
sigmoide t = 1/ (1 + exp (-t))

-- | controlla lo strappo dei movimenti (valori > 0)
type Accelerazione = Float

-- | costruisce una sequenza di grafi che interpolano i grafi dati, assegnando il numero di passi indicato per ogni passaggio
film 	:: Accelerazione 	-- ^ strappo dei movimenti
	-> [(Int,Tree Semiretta)] -- ^ sequenza di configurazioni con i passi necessari a raggiungerla dalla precedente
	-> [Tree Semiretta]	-- ^ sequenza di grafi
film l xs = do
	((_,x),(n,y)) <- zip xs (tail xs)
	interpola (mkInterpolazione x y) x . sigmoide <$>  take n [-l/2,(-l/2) + l/fromIntegral n..]
{-
regolaAngolo :: Angolo -> Tree Semiretta -> Tree Semiretta
regolaAngolo alpha = movimento id () . modifyNode f . fmap g where
	f (Semiretta p y,_) = (Semiretta p y, \() -> y + alpha) 
	g s@(Semiretta p y) = (s,const y)
-- | imposta un angolo di una specifica semiretta in un grafo di semirette
configura 	:: Angolo 	-- ^ valore assoluto dell'angolo in gradi
		-> TreePath 	-- ^ percorso per la semiretta nel grafo , contando le dipendenze da sinistra
		-> Tree Semiretta 	-- ^ grafo di semirette da correggere
		-> Tree Semiretta	-- ^ grafo corretto
configura alpha tp tr = replaceTreeNode (regolaAngolo alpha) tp tr
-}

