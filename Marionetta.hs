module Marionetta where

import Graphics.Gloss
import Data.Tree (Tree (Node))
import Model
import Render.Gloss
import Interface
import Render

-------------------------- esempio --------------------------------

pezzo x y t l u = Figura (flip setCenter (t,p) $ mkRenderer) 
		 (Scale (1/u) 1 $  ThickCircle l (l/4))
                 (Semiretta p t) where  p = Punto (x,y) 
						   
simmetrico (Figura renderer picture (Semiretta (Punto (x,y)) al)) 
	= Figura renderer picture (Semiretta (Punto (-x,y)) (180-al))


testa 		= pezzo 0 10 0 20 4
corpo		= pezzo 0 50 270 40 2
bracciodx 	= pezzo 15 (-5) 300 25 3.5
avambracciodx 	= pezzo 30 (-50) 325 20 3.5
cosciadx 	= pezzo 10 (-80) 300 30 3
gambadx 	= pezzo 30 (-55) 270 27 3

marionetta' :: Tree Figura 
marionetta' = Node corpo 
	[	Node testa []
	,	Node bracciodx [Node avambracciodx []]
	,	Node (simmetrico bracciodx) [Node (simmetrico avambracciodx) []]
	,	Node cosciadx [Node gambadx []]
	,	Node (simmetrico cosciadx) [Node (simmetrico gambadx) []]
	]

marionetta = Node testa []

-- main = animateInWindow "marionetta" (300,300) (0,0) white $ renderFilm pezzi (film 5 animazione) 3
-- main = run pezzi semirette
