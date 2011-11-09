{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE ImpredicativeTypes #-}

module Interfaccia where


import Linguaggio
       ( Passo(..), deserializza, Sequenza(..) , Serializzazione(..))
import Graphics.Gloss
       (greyN, color, green, line, displayInWindow, Picture, white,
        gameInWindow)
import Model (Relativo, Pezzo(..), Figura, renderFigura, Rendering)
import Graphics.Gloss.Interface.Game (Event)
import Graphics.Gloss.Data.Picture (Picture(..))
import Debug.Trace
import Data.Zip (Selector)
import Data.Tree (Tree)
import Data.List.Zipper (valore, elementi, Zipper(..))


data IFigura = IFigura
    {   ifigura :: Figura
    ,   irotazione :: Selector Tree (Pezzo Relativo)
    ,   ipov :: Figura -> Figura
    }

type Cardini = Zipper IFigura


data World = World
    {   cardini :: Cardini
    ,   renderering :: Rendering Picture
    }

-----------------------------   rendering ---------------------------------------------
renderIFigura re = Pictures . renderFigura re . ifigura
croce = Color green $ Pictures [line [(-200,0),(200,0)], line [(0,200),(0,-200)]]
renderWorld :: World -> Picture
renderWorld (World ca re ) = let
    ps =  Pictures . map (renderIFigura re) $ elementi  ca
    actual = (renderIFigura re) $ valore ca
    in Pictures [croce,color (greyN 0.5) ps, color (greyN 0.1) actual]



-----------------------------  input ---------------------------------------------------

changeWorld :: Event -> World -> World
changeWorld = const id


----------------------------- time -----------------------------------------------------
stepWorld :: Float -> World -> World
stepWorld = const id


run :: World -> IO ()
run world = gameInWindow "marionetta" (600,600) (0,0) white 100 world renderWorld changeWorld stepWorld

