module Run (Run, run, Geometrici (..), Grafici (..), Descrizione, Implementazione (..)) where

import Data.Tree (Tree)
import Data.Monoid (Monoid)
import Model (Pezzo (Pezzo), Punto (Punto), Assoluto, relativizza)

import View (Render, Colore, RenderHelp, renderWorld)
import Controller (World(World), Evento, mkWorld, catchEvento, updateTime)


data Geometrici = Ge
    {    centroRotazioneX :: Float
    ,    centroRotazioneY :: Float
    ,    origineX :: Float
    ,    origineY :: Float
    ,    angolo    :: Float
    }

data Grafici = Gr
    {    lunghezza :: Float
    ,     schiacciamento :: Float
    }


type Descrizione = Tree (Geometrici, Grafici)

mkPezzo :: Geometrici -> Pezzo Assoluto 
mkPezzo (Ge x y xo yo beta) = (Pezzo (Punto (x,y)) (Punto (xo, yo)) alpha) where
                alpha = beta * pi / 180

data Implementazione b e = Implementazione
    {    mkSprite :: Grafici -> Render b
    ,    colora :: Colore b
    ,    mostraAiuto :: RenderHelp b 
    ,    catturaEvento :: e -> Evento
    }

type Run b e = World -> (World -> b) -> (e -> World -> World) -> (Float -> World -> World) -> IO ()

run :: Monoid b => Run b e -> Implementazione b e -> Descrizione -> IO ()
run f (Implementazione mks co mo ca) de = f w rew cat updateTime where
    rew = renderWorld co mo (fmap (mks . snd) de) 
    cat = catchEvento . ca 
    w = mkWorld (relativizza $ fmap (mkPezzo . fst) de)

