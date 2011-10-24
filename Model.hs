module Model (Punto (..), Semiretta (..), Angolo , TreePath, Tree, Accelerazione, configura, film) where

import Data.Tree (Tree)
import Data.Tree.Missing (replaceTreeNode, zipTreeWith, recurseTreeAccum, TreePath, modifyNode)
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
data Semiretta = Semiretta Punto Angolo deriving (Show,Read)

type Tempo = Float

type Spiazzamento = Punto -> Punto

data Movimento = Movimento Spiazzamento Spiazzamento

movimento = movimento' (Movimento id id) where	
	movimento' :: Movimento -> a -> Tree (Semiretta, a -> Angolo) ->  Tree Semiretta
	movimento' m w tr = recurseTreeAccum m f tr where
		f (Movimento sposta gira) (Semiretta p y, a) = (Movimento sposta' (ruota p (a' - y)), Semiretta (sposta' p) a')
			where 	sposta' = sposta . (+ (p' - p))
				p' = gira p
				a' = a w

regolaAngolo :: Angolo -> Tree Semiretta -> Tree Semiretta
regolaAngolo alpha = movimento () . modifyNode (\(Semiretta p y,_) -> (Semiretta p y, \() -> y + alpha)) . fmap (\s@(Semiretta p y) -> (s,const y)) 

-- | imposta un angolo di una specifica semiretta in un grafo di semirette
configura 	:: Angolo 	-- ^ valore assoluto dell'angolo in gradi
		-> TreePath 	-- ^ percorso per la semiretta nel grafo , contando le dipendenze da sinistra
		-> Tree Semiretta 	-- ^ grafo di semirette da correggere
		-> Tree Semiretta	-- ^ grafo corretto
configura alpha tp tr = replaceTreeNode (regolaAngolo alpha) tp tr

interpola :: Tree Semiretta -> Tree Semiretta -> Tree(Semiretta, Tempo -> Angolo)
interpola t1 t2  = zipTreeWith f t1 t2 where
	f (Semiretta p x) (Semiretta _ y) = (Semiretta p x , \t -> x + t*(y - x))

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
	flip movimento (interpola x y) . sigmoide <$>  take n [-l/2,(-l/2) + l/fromIntegral n..]

