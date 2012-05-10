

import Data.Tree (Tree (Node))
import Control.Arrow (first)

import Run (Descrizione, run, Grafici(Gr), Geometrici(Ge))
import Gloss (gloss_run, gloss_implementazione)

testa   	= (Ge 0 60 0 80 0, 		Gr 20 1.5)
corpo   	= (Ge 0 0 0 20 0, 		Gr 40 2)
bracciodx     	= (Ge 13 50 13 25 0, 		Gr 25 3.5)
avambracciodx   = (Ge 13 0 13 (-20) 0, 		Gr 20 3.5)
cosciadx     	= (Ge 10 (-10) 10 (-40) 0, 	Gr 30 3)
gambadx     	= (Ge 10 (-70) 10 (-97) 0, 	Gr 27 3)

simmetrico (Ge x y xo yo alpha, gr)  =  (Ge (-x) y (-xo) yo (pi - alpha), gr)

marionetta :: Descrizione 
marionetta = Node corpo
    [    Node testa []
    ,    Node bracciodx [Node avambracciodx []]
    ,    Node (simmetrico bracciodx) [Node (simmetrico avambracciodx) []]
    ,    Node cosciadx [Node gambadx []]
    ,    Node (simmetrico cosciadx) [Node (simmetrico gambadx) []]
    ]

main = run (gloss_run "marionetta" (600,600) (0,0)) gloss_implementazione marionetta
