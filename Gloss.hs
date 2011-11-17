
{-# LANGUAGE ViewPatterns #-}
module Gloss where


import Prelude hiding (catch)
import Data.Monoid (mconcat)

import Graphics.Gloss
import Graphics.Gloss.Interface.Game

import View
import Model 
import Controller 
import Run
import Debug.Trace
glCatch :: Event -> Evento

glCatch (EventMotion (Punto -> p)) = Puntatore p
glCatch (EventKey (Char 'r') Down _ (Punto -> p)) = Rotazione p Inizio 
glCatch (EventKey (Char 'r') Up _ (Punto -> p)) = Rotazione p Fine 
glCatch (EventKey (Char 't') Down _ (Punto -> p)) = Traslazione p Inizio 
glCatch (EventKey (Char 't') Up _ (Punto -> p)) = Traslazione p Fine 
glCatch (EventKey (Char 'x') Down _ (Punto -> p)) = SpostamentoCentro p Inizio 
glCatch (EventKey (Char 'x') Up _ (Punto -> p)) = SpostamentoCentro p Fine 
glCatch (EventKey (Char 'd') Down _ _ ) = Cancella
glCatch (EventKey (Char 'c') Down _ _ ) = Clona
glCatch (EventKey (MouseButton WheelUp) Up _ _) = Fuoco Destra
glCatch (EventKey (MouseButton WheelDown) Up _ _) = Fuoco Sinistra
glCatch (EventKey (Char 'g') Down _ (Punto -> p)) = Ricentra p
glCatch (EventKey (Char 's') Down _ (Punto -> p)) = Seleziona p
glCatch (EventKey (SpecialKey KeySpace) Down _ _) = Deseleziona
glCatch _ = Silent


colore :: Colore Picture 
colore (r,g,b) = Color (makeColor r g b 1) 

renderHelp :: RenderHelp Picture
renderHelp help = mconcat [Color blue . translate (-250) (250-16*i) . scale 0.09 0.14 $ Text h | (i,h) <- zip [0..] help]

elemento :: Grafici -> Picture
elemento (Gr l u) = Scale (1/u) 1 $  Circle l

renderPezzo :: Picture -> Render Picture
renderPezzo pc (Pezzo (Punto (cx,cy)) (Punto (ox,oy)) alpha ) = Pictures
    [   translate ox oy . rotate (-alpha * 180 / pi) $ pc
    ,   translate cx cy . color yellow $ Circle 3
    ]

render :: Grafici -> Render Picture
render = renderPezzo . elemento

gloss_implementazione :: Implementazione Picture Event
gloss_implementazione = Implementazione render colore renderHelp glCatch

gloss_run :: String -> (Int,Int) -> (Int,Int) -> Run Picture Event
gloss_run s c l w rew ce = gameInWindow s c l white 0 w rew ce (const id)
