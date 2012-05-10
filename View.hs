{-# LANGUAGE ViewPatterns #-}
module View (RenderHelp, Render, Colore, renderWorld) where

import Prelude hiding (zipWith)
import Data.Tree (Tree) 
import Data.Foldable (toList)
import Data.Monoid (mconcat,Monoid,mempty)


import Data.Zip (zipWith)
import Data.Tree.Missing (modifyTop, routingDumb)
import Data.List.Zipper (Zipper, elementi, valore, destra, sinistra, isLast)
import Model (assolutizza , Pezzo , Assoluto, Figura, Tempo, Normalizzato, routingPezzi, rotazioneInOrigine)
import IFigura (IFigura(IFigura))
import Controller (World (..))
import Movie

type Render b = Pezzo Assoluto -> b

renderFigura :: Monoid b => Tree (Render b) -> Figura -> b
renderFigura r x =  mconcat . toList . zipWith ($) r . assolutizza $ x

type Colore b = (Float,Float,Float) -> b -> b

-- colori vari
selezionato = (0,1,1)
top = (0,0,1)
text = (0,1,0)

renderIFigura :: Monoid b => Colore b -> Tree (Render b) -> IFigura -> b
renderIFigura co re (IFigura ifig isels iforw _ ) = renderFigura re'' ifig
    where
    re' = foldr (\ir re -> fst (ir re) $ (co selezionato .)) (routingDumb iforw re) isels
    re'' = modifyTop (co top .) re'

type RenderHelp b = [String] -> b


film = (0.7,0.7,0.7)

renderMovie :: Monoid b => Tree (Render b) -> Tempo Normalizzato -> ((IFigura, Fulcrum), (IFigura, Fulcrum)) -> b
renderMovie re t ((IFigura ifig _ _ back,fu), (IFigura ifig2 _ _ back2,_)) = let 
	ifig' =  rotazioneInOrigine . routingPezzi undefined back $ assolutizza ifig
	ifig'' =  rotazioneInOrigine . routingPezzi undefined back2 $ assolutizza ifig2
	in   renderFigura re . generaPasso ifig' ifig'' fu $ t 

renderWorld :: Monoid b => Colore b -> RenderHelp b -> Tree (Render b) -> World  -> b
renderWorld co he re (World t z _) = let
    xs = elementi z
    ms = co film . mconcat . map (renderMovie re t) . zip xs $ tail xs
    actual = renderIFigura co re . fst . valore $ z
    in mconcat [co text $ he help,  actual, ms]

help =  [   "S: select/deselect nearest to pointer piece for rotation"
        ,   "Space: deselect all pieces"
        ,   "R: rotate selected pieces while moving the mouse"
        ,   "X: move top piece rotation while moving the mouse"
        ,   "G: change top piece as the nearest to pointer"
        ,   "T: translate marionetta while moving the mouse"
        ,   "C: clone marionetta"
        ,   "Mouse wheel: select a marionetta to edit"
        ,   "D: eliminate marionetta"
        ]


