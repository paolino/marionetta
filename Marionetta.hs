import Graphics.Gloss
import Data.Tree (Tree (Node))
import Model
import Render.Gloss
import Interface

-------------------------- esempio --------------------------------

stecco l u (Semiretta (Punto (x,y)) al) 
		= Translate x y
		. Rotate (270-al)
		. Translate 0 (-l) 
		. Scale (1/u) 1 $  ThickCircle l (l/3)

simmetrico (Semiretta (Punto (x,y)) al,pc) = (Semiretta (Punto (-x,y)) (180-al),pc)

pezzo x y angolo lun lente = (Semiretta (Punto (x,y)) angolo, stecco lun lente)

testa 		= pezzo 0 60 90 20 1
corpo		= pezzo 0 50 270 40 2
bracciodx 	= pezzo 15 45 300 25 3.5
avambracciodx 	= pezzo 45 (-5) 325 20 3.5
cosciadx 	= pezzo 10 (-30) 300 30 3
gambadx 	= pezzo 40 (-85) 270 27 3



marionetta :: Tree (Semiretta, Semiretta -> Picture)
marionetta = Node corpo 
	[	Node testa []
	,	Node bracciodx [Node avambracciodx []]
	,	Node (simmetrico bracciodx) [Node (simmetrico avambracciodx) []]
	,	Node cosciadx [Node gambadx []]
	,	Node (simmetrico cosciadx) [Node (simmetrico gambadx) []]
	]
semirette = fmap fst marionetta
pezzi = fmap snd marionetta

m1 = configura (100) [1,0]. configura (-100) [2,0]. configura (120) [1]. configura (-120) [2] $ semirette
m2 = configura (-60) [3] m1

m3 = configura (-100) [1]. configura (-175) [1,0] $m2
m3'5 = configura (-30) [] m3
m3'7 = configura (60) [] m3'5
m4 = configura (-45) [1] m3'7

animazione :: [(Int,Tree Semiretta)]
animazione = 
	[	(0,semirette)
	,	(50,m1)
	,	(50,m2)
	,	(50,m3)
	,	(50,m3'5)
	,	(50,m3'7)
	,	(50,m4)
	,	(50,semirette)
	]

tempo = 5
fluido = 8
-- main = animateInWindow "marionetta" (300,300) (0,0) white $ renderFilm pezzi (film 5 animazione) 3
main = run pezzi semirette
