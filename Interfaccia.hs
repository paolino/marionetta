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
       (Punto(..), Relativo, Pezzo(..), Figura, renderFigura, Rendering)

import Graphics.Gloss.Data.Picture (Picture(..))
import Debug.Trace
import Data.Zip (Selector)
import Data.Tree (Tree)
import Data.List.Zipper
       (modifica, inserisci, valore, elementi, Zipper(..))
import Graphics.Gloss.Interface.Game
import Data.Graph (reachable)
import Data.Tree.Missing (modifyTop)
import Interface.Register (register, catchRegister, CatchEvent, Movimenti, mkMovimenti)


data IFigura = IFigura
    {   ifigura :: Figura
    ,   irotazione :: Selector Tree (Pezzo Relativo)
    ,   ipov :: Figura -> Figura
    }




-----------------------------   rendering ---------------------------------------------
renderIFigura re = Pictures . renderFigura re . ifigura

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



changeWorld :: CatchEvent (Zipper IFigura, Movimenti (Zipper IFigura))

changeWorld (EventKey (Char 'c') Down _ _ ) (z, mov)  = Just (inserisci id z, mov)
changeWorld e z = catchRegister [registraT] e z


changeWorldG e w = maybe w id $ changeWorld e w



----------------------------- time -----------------------------------------------------
stepWorld :: Float -> a -> a
stepWorld = const id

type World = Zipper IFigura

run :: Rendering Picture -> World -> IO ()
run re world = gameInWindow "marionetta" (600,600) (0,0) white 100 (world,mkMovimenti) (renderWorldG re) changeWorldG stepWorld

