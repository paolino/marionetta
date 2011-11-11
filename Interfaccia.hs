{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}

module Interfaccia where


import Linguaggio
       ( Passo(..), deserializza, Sequenza(..) , Serializzazione(..))
import Graphics.Gloss
       (greyN, color, green, line, displayInWindow, Picture, white,
        gameInWindow)
import Model
       (assolutizza, vicino, Angolo, ruotaScelto, Punto(..), Relativo,
        Pezzo(..), Figura, renderFigura, Rendering)

import Graphics.Gloss.Data.Picture (Picture(..))
import Debug.Trace
import Data.Zip (Selector)
import Data.Tree (Tree)
import Data.List.Zipper
       (elimina, destra, sinistra, modifica, inserisci, valore, elementi,
        Zipper(..))
import Graphics.Gloss.Interface.Game
import Data.Graph (reachable)
import Data.Tree.Missing (modifyTop)
import Interface.Register (register, catchRegister, CatchEvent, Movimenti, mkMovimenti)
import Control.Arrow (ArrowChoice(..))


data IFigura = IFigura
    {   ifigura :: Figura
    ,   irotazione :: forall b . Selector Tree b
    ,   ipov :: Figura -> Figura
    }




-----------------------------   rendering ---------------------------------------------
renderIFigura re (IFigura ifig ir ipov) = Pictures . renderFigura re' $ ifig
    where
    re' = fst (ir re) $ (Color yellow .)


croce = Color green $ Pictures [line [(-200,0),(200,0)], line [(0,200),(0,-200)]]
renderWorld :: Rendering Picture -> Zipper IFigura -> Picture
renderWorld re ca  = let
    ps =  Pictures . map (renderIFigura re) $ elementi  ca
    actual = (renderIFigura re) $ valore ca
    in Pictures [croce,color (greyN 0.5) ps, color (greyN 0.1) actual]

renderWorldG re = renderWorld re . fst

-----------------------------  input ---------------------------------------------------

registraT :: CatchEvent (Movimenti (Zipper IFigura))

registraT = register (Char 't') $ \ l z l' -> let
        f (IFigura ifig ir ipov) = let
            ifig' = modifyTop g ifig
            g (Pezzo p o alpha) = Pezzo (p + l' - l) o alpha
            in IFigura ifig' ir ipov
        in modifica f z
registraR :: CatchEvent (Movimenti (Zipper IFigura))
registraR = register (Char 'r') $ \ l z l' -> let
        f (IFigura ifig ir ipov) = let
            ifig' = ruotaScelto ir alpha ifig
            Pezzo q _ _ = head . snd $ ir (assolutizza ifig)
            alpha = atan2 y' x' - atan2 y x
            Punto (x,y) = l - q
            Punto (x',y') = l' - q
            in IFigura ifig' ir ipov
        in modifica f z
registraS :: CatchEvent (Movimenti (Zipper IFigura))
registraS = register (Char 's') $ \ l z l' -> let
        f (IFigura ifig ir ipov) = let
            ir' = vicino l' (assolutizza ifig)
            in IFigura ifig ir' ipov
        in modifica f z
changeWorld :: CatchEvent (Zipper IFigura, Movimenti (Zipper IFigura))

changeWorld (EventKey (Char 'c') Down _ _ ) (z, mov)  = Just (inserisci id z, mov)
changeWorld (EventKey (Char 'd') Down _ _ ) (z, mov)  = Just (maybe z id $ elimina z, mov)
changeWorld (EventKey (MouseButton WheelUp) Up _ _ ) (z, mov)  = Just (destra z, mov)
changeWorld (EventKey (MouseButton WheelDown) Up _ _ ) (z, mov)  = Just (sinistra z, mov)
changeWorld e z = catchRegister [registraT, registraR, registraS] e z


changeWorldG e w = maybe w id $ changeWorld e w



----------------------------- time -----------------------------------------------------
stepWorld :: Float -> a -> a
stepWorld = const id

type World = Zipper IFigura

run :: Rendering Picture -> World -> IO ()
run re world = gameInWindow "marionetta" (600,600) (0,0) white 100 (world,mkMovimenti) (renderWorldG re) changeWorldG stepWorld



