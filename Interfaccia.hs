{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Interfaccia where


-- import Linguaggio
  --     ( Passo(..), deserializza, Sequenza(..) , Serializzazione(..))
import Graphics.Gloss
       (greyN, color, green, line, displayInWindow, Picture, white,
        gameInWindow)
import Model
       (Assoluto, relativizza, assolutizza, vicino, Angolo, ruotaScelto,
        Punto(..), Relativo, Pezzo(..), Figura, renderFigura, Rendering)

import Graphics.Gloss.Data.Picture (Picture(..))
import Debug.Trace
import Data.Zip (Selector, moveSelector, filterDuplicates, labella)
import Data.Tree (drawTree, Tree)
import Data.List.Zipper
       (elimina, destra, sinistra, modifica, inserisci, valore, elementi,
        Zipper(..))
import Graphics.Gloss.Interface.Game
import Data.Tree.Missing
import Interface.Register (register, catchRegister, CatchEvent, Movimenti, mkMovimenti)
import Control.Arrow (ArrowChoice(..))
import Control.Exception (assert)


data IFigura = IFigura
        {   ifigura :: Figura
        ,   iselectors :: forall b. [Selector Tree b]
        ,   iforward :: forall b . Routing b
        ,   ibackward :: forall b . Routing b

        }



-----------------------------   rendering ---------------------------------------------
renderIFigura re (IFigura ifig isels iforw _ ) = Pictures .  renderFigura re'' $ ifig
    where
    re' = foldr (\ir re -> fst (ir re) $ (Color yellow .)) (routingDumb iforw re) isels
    re'' = modifyTop (Color blue .) re'


croce = Color green $ Pictures [line [(-200,0),(200,0)], line [(0,200),(0,-200)]]

renderWorld :: Rendering Picture -> Zipper IFigura -> Picture

renderWorld re ca  = let
    ps =  Pictures . map (renderIFigura re) $ elementi  ca
    actual = (renderIFigura re) $ valore ca
    in Pictures $[Color blue . translate (-250) (250-16*i) . scale 0.09 0.14 $ Text h | (i,h) <- zip [0..] help] ++
        [ croce,color (greyN 0.5) ps, color (greyN 0.1) actual]

renderWorldG re = renderWorld re . fst

help =  [   "S: select/deselect nearest to pointer piece for rotation"
        ,   "Z: deselect all pieces"
        ,   "R: rotate selected pieces while moving the mouse"
        ,   "X: move top piece rotation while moving the mouse"
        ,   "G: change top piece as the nearest to pointer"
        ,   "T: translate marionetta while moving the mouse"
        ,   "C: clone marionetta"
        ,   "Mouse wheel: select a marionetta to edit"
        ,   "D: eliminate marionetta"
        ]
stepWorld :: Float -> a -> a
stepWorld = const id

type World = Zipper IFigura

-- run :: Rendering Picture -> World -> IO ()
-- run re world = gameInWindow "marionetta" (600,600) (0,0) white 100 (world,mkMovimenti) (renderWorldG re) changeWorldG stepWorld




