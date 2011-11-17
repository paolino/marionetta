module View where

import Prelude hiding (zipWith)
import Data.Tree (Tree) 
import Data.Foldable (toList)
import Data.Monoid (mconcat,Monoid)


import Data.Zip (zipWith)
import Data.Tree.Missing (modifyTop, routingDumb, Routing)
import Data.List.Zipper (Zipper, elementi, valore)
import Model (assolutizza , Pezzo (..), Punto (..), Assoluto, Figura)
import IFigura (IFigura(..))


type Render b = Pezzo Assoluto -> b

renderFigura :: Monoid b => Tree (Render b) -> Figura -> b
renderFigura r x =  mconcat . toList . zipWith ($) r . assolutizza $ x

type Colore b = (Float,Float,Float) -> b -> b

selezionato = (0,1,1)
top = (0,0,1)
text = (0,1,0)

renderIFigura :: Monoid b => Colore b -> Tree (Render b) -> IFigura -> b
renderIFigura co re (IFigura ifig isels iforw _ ) = renderFigura re'' ifig
    where
    re' = foldr (\ir re -> fst (ir re) $ (co selezionato .)) (routingDumb iforw re) isels
    re'' = modifyTop (co top .) re'

type RenderHelp b = [String] -> b

renderWorld :: Monoid b => Colore b -> RenderHelp b -> Tree (Render b) -> Zipper IFigura -> b
renderWorld co he re ca  = let
    ps =  mconcat . map (renderIFigura co re) $ elementi ca
    actual = renderIFigura co re . valore $ ca
    in mconcat [co text $ he help, co (0.5,0.5,0.5) ps, co (0.1,0.1,0.1) actual]

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


