
import Debug.Trace
import Graphics.Gloss
import Data.Tree (Tree (Node))
import Data.Tree.Missing (ricentratore, labella)
import Model (Figura, Rendering, Renderer,renderFigura, Punto (..), Pezzo (..), assolutizza, relativizza, Assoluto, vicino )
import Interfaccia
import Control.Arrow
import Linguaggio (Serializzazione(..) , Passo (..))
import Data.List.Zipper


-------------------------- esempio --------------------------------

elemento :: Float -> Float -> Picture
elemento l u = Scale (1/u) 1 $  Circle l

-- simmetrico :: Pezzo Relativo -> Pezzo Relativo
simmetrico (Pezzo (Punto (x,y)) (Punto (xo,yo)) alpha)  =  Pezzo (Punto (-x,y)) (Punto (-xo,yo)) (pi - alpha)


pezzo :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> (Pezzo Assoluto, Picture)
pezzo x y xo yo beta l u  = (Pezzo (Punto (x,y)) (Punto (xo, yo)) alpha ,elemento l u ) where
                alpha = beta * pi / 180

renderer :: Picture -> Renderer Picture
renderer pc (Pezzo (Punto (cx,cy)) (Punto (ox,oy)) alpha ) = Pictures
    [   translate ox oy . rotate (-alpha * 180 / pi) $ pc
    ,   translate cx cy . color yellow $ Circle 3
    ]


testa   = pezzo 0 60 0 80 0 20 1.5
corpo   = pezzo 0 0 0 20 0 40 2
bracciodx     = pezzo 13 50 13 25 0 25 3.5

avambracciodx     = pezzo 13 0 13 (-20) 0 20 3.5

cosciadx     = pezzo 10 (-10) 10 (-40) 0 30 3
gambadx     = pezzo 10 (-70) 10 (-97) 0 27 3

-- marionetta :: Figura
marionetta = Node corpo
    [    Node testa []
    ,    Node bracciodx [Node avambracciodx []]
    ,    Node (first simmetrico bracciodx) [Node (first simmetrico avambracciodx) []]
    ,    Node cosciadx [Node gambadx []]
    ,    Node (first simmetrico cosciadx) [Node (first simmetrico gambadx) []]
    ]

rendering :: Rendering Picture
rendering = fmap (renderer . snd) $ marionetta

figura :: Figura
figura = relativizza $ fmap fst marionetta

world :: World
world = mkZipper $ IFigura figura (vicino (Punto (0,0)) (assolutizza figura)) id

main = run rendering world


