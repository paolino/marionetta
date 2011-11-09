
import Debug.Trace
import Graphics.Gloss
import Data.Tree (Tree (Node))
import Data.Tree.Missing (ricentratore, labella)
import Model
import Interfaccia
import Linguaggio
       (Serializzazione(..), Figura, Rendering, Renderer, Passo (..), renderFigura)


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


testa   = pezzo 0 60 0 78 0 18 1.5
corpo   = pezzo 0 0 0 20 0 40 2
{-bracciodx     = pezzo 15 (-5) 300 25 3.5
avambracciodx     = pezzo 30 (-50) 325 20 3.5
cosciadx     = pezzo 10 (-80) 300 30 3
gambadx     = pezzo 30 (-55) 270 27 3

marionetta' :: Tree Figura
marionetta' = Node corpo
    [    Node testa []
    ,    Node bracciodx [Node avambracciodx []]
    ,    Node (simmetrico bracciodx) [Node (simmetrico avambracciodx) []]
    ,    Node cosciadx [Node gambadx []]
    ,    Node (simmetrico cosciadx) [Node (simmetrico gambadx) []]
    ]
-}

marionetta = Node corpo [Node testa []]

rendering :: Rendering Picture
rendering = fmap (renderer . snd) $ marionetta

figura :: Figura
figura = relativizza $ fmap fst marionetta

rice  n p  = relativizza
        . ricentratore n (labella [0..] figura) (Pezzo (Punto p) undefined undefined) gp
        . assolutizza $ figura where
    gp :: Pezzo Assoluto -> Pezzo Assoluto -> Pezzo Assoluto
    gp (Pezzo c _ _) (Pezzo _ o alpha) = Pezzo c o alpha

pictures' = concatMap (renderFigura rendering)
    [figura
    , rice 0 (10,5)
    , ruotaScelto (vicino (Punto (0,0)) (assolutizza figura)) (pi/16) $ rice 0 (10,5)
--    , relativizza . assolutizza $ figura
    ]
world :: World
world = World (Serializzazione figura
        [   Passo figura (Tempo  0) 0 (Punto (0,0))
        ,   Passo (ruotaScelto (vicino (Punto (0,0)) (assolutizza figura)) (pi/16) figura) (Tempo 0) 0 (Punto (30,30))
        ,   Passo figura (Tempo  0) 0 (Punto (0,0))
        ]
    )
    Nothing
    rendering



-- main = animateInWindow "marionetta" (300,300) (0,0) white $ renderFilm pezzi (film 5 animazione) 3
-- main = run world
main = displayInWindow "marionetta" (300,300) (0,0) white $ Pictures pictures'


