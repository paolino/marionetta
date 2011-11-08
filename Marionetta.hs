
import Debug.Trace
import Graphics.Gloss
import Data.Tree (Tree (Node))
import Model
import Interfaccia
import Linguaggio
       (Serializzazione(..), Figura, Rendering, Renderer, Passo (..))


-------------------------- esempio --------------------------------

elemento :: Float -> Float -> Picture
elemento l u = Scale (1/u) 1 $  Circle l

-- simmetrico :: Pezzo Relativo -> Pezzo Relativo
simmetrico (Pezzo (Punto (x,y)) (Left (Punto (xo,yo)))) =  Pezzo (Punto (-x,y)) $ Left (Punto (-xo,yo))

pezzo :: Float -> Float -> Float -> Float -> Float -> Float -> (Pezzo Assoluto, Picture)
pezzo x y d beta l u  =  (Pezzo (Punto (x,y)) o, Rotate beta $ elemento l u ) where
                o = case d > 0 of
                    True -> Left $ Punto (x + d*cos alpha, y + d*sin alpha)
                    False -> Right alpha
                alpha = beta * pi / 180

renderer :: Picture -> Renderer Picture
renderer pc ((Punto (ox,oy),alpha),Punto (px,py)) = Pictures [translate ox oy . rotate (-alpha * 180 / pi) $ pc,
    translate px py . color yellow $ Circle 3]


testa   = pezzo 0 60 18 90 18 1.5
corpo   = pezzo 0 0 20 90 40 2
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

world :: World
world = World (Serializzazione figura
        [   Passo figura (Tempo  0) 0 (Punto (0,0))
        ,   Passo (ruotaScelto (vicino (Punto (0,0)) (assolutizza figura)) (pi/8) figura) (Tempo 0) 0 (Punto (30,30))
        ,   Passo figura (Tempo  0) 0 (Punto (0,0))
        ]
    )
    Nothing
    rendering



-- main = animateInWindow "marionetta" (300,300) (0,0) white $ renderFilm pezzi (film 5 animazione) 3
main = run world


