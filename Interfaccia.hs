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
import qualified Data.Char
import Graphics.Gloss.Interface.Game
import Data.Graph (reachable)
import Data.Tree.Missing
       (Routing, backward, forward, Ricentratore, modifyTop)
import Interface.Register (register, catchRegister, CatchEvent, Movimenti, mkMovimenti)
import Control.Arrow (ArrowChoice(..))
import Control.Exception (assert)


data IFigura = IFigura
        {   ifigura :: Figura
        ,   iselectors :: forall b. [Selector Tree b]
        ,   iforward :: forall b . Routing b
        ,   ibackward :: forall b . Routing b

        }

routingPezzi :: Punto -> Routing (Pezzo Assoluto) -> Tree (Pezzo Assoluto) -> Tree (Pezzo Assoluto)
routingPezzi p r = snd . r (Pezzo p undefined undefined) (\(Pezzo c _ _) (Pezzo _ o alpha) -> Pezzo c o alpha)

routingDumb :: Routing b -> Tree b -> Tree b
routingDumb r = snd . r undefined (const id)

rotazioneInOrigine = modifyTop $ \(Pezzo _ o alpha) ->  Pezzo o o alpha

ricentra :: Punto -> IFigura -> IFigura
ricentra l (IFigura ifig isels _ ibackw ) = let

            ifig' = rotazioneInOrigine . routingPezzi undefined ibackw $ assolutizza ifig

            isels' = map (moveSelector ifig $ routingDumb ibackw) isels
            ir = vicino l ifig'
            lifig = labella [0..] $ ifig'
            c = head $ snd (ir lifig)
            iforw =  forward c lifig
            ibackw' =  backward c lifig
            ifig'' = relativizza . rotazioneInOrigine . routingPezzi undefined iforw $ ifig'
            isels'' = map (moveSelector ifig' $ routingDumb iforw) isels'
            in  IFigura ifig'' isels'' iforw ibackw'


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
-----------------------------  input ---------------------------------------------------

type CatchWorld = CatchEvent (Zipper IFigura, Movimenti (Zipper IFigura))

registerModActual :: Data.Char.Char
    -> (Punto -> Punto -> IFigura -> IFigura)
    -> CatchWorld
registerModActual s f = register (Char s) $ \ l z l' -> modifica (f l l') z

registraT :: CatchWorld
registraT = registerModActual 't' $ \ l l' (IFigura ifig ir iforw ibackw) -> let
            ifig' = modifyTop g ifig
            g (Pezzo p o alpha) = Pezzo (p + l' - l) o alpha
            in IFigura ifig' ir iforw ibackw

registraR :: CatchWorld
registraR = registerModActual 'r' $ \ l l' (IFigura ifig ir iforw ibackw ) -> let
            ifig' = foldr (\(ir,alpha) -> ruotaScelto ir alpha) ifig (zip ir $ map iralpha ir)
            iralpha ir = let
                Pezzo q _ _ = head . snd $ ir (assolutizza ifig)
                alpha = atan2 y' x' - atan2 y x
                Punto (x,y) = l - q
                Punto (x',y') = l' - q
                in alpha
            in IFigura ifig' ir iforw ibackw
registraX :: CatchWorld

registraX = registerModActual 'x' $ \ l l' (IFigura ifig ir iforw ibackw)
        -> IFigura (relativizza . modifyTop (\(Pezzo _ o alpha) -> Pezzo l o alpha)
             . assolutizza $ ifig) ir iforw ibackw




changeWorld :: CatchEvent (Zipper IFigura, Movimenti (Zipper IFigura))

changeWorld (EventKey (Char 'c') Down _ _ ) (z, mov)  = Just (inserisci id z, mov)
changeWorld (EventKey (Char 'd') Down _ _ ) (z, mov)  = Just (maybe z id $ elimina z, mov)
changeWorld (EventKey (MouseButton WheelUp) Up _ _ ) (z, mov)  = Just (destra z, mov)
changeWorld (EventKey (MouseButton WheelDown) Up _ _ ) (z, mov)  = Just (sinistra z, mov)

changeWorld (EventKey (Char 'g') Down _ (Punto -> l)) (z, mov)  = Just (modifica (ricentra l) z, mov)
changeWorld (EventKey (Char 'z') Down _ _) (z, mov)  = Just (modifica (\(IFigura ifig _ iforw ibackw) ->
                        IFigura ifig [] iforw ibackw) z, mov)


changeWorld (EventKey (Char 's') Down _ (Punto -> l')) (z,mov) = Just (modifica f z, mov) where
    f (IFigura ifig ir iforw ibackw) = let
            ir' = vicino l' (assolutizza ifig)
            in IFigura ifig (filterDuplicates ifig (ir':ir)) iforw ibackw

changeWorld e z = catchRegister [registraT, registraR, registraX] e z


changeWorldG e w = maybe w id $ changeWorld e w



----------------------------- time -----------------------------------------------------
stepWorld :: Float -> a -> a
stepWorld = const id

type World = Zipper IFigura

run :: Rendering Picture -> World -> IO ()
run re world = gameInWindow "marionetta" (600,600) (0,0) white 100 (world,mkMovimenti) (renderWorldG re) changeWorldG stepWorld




