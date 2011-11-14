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
       (relativizza, assolutizza, vicino, Angolo, ruotaScelto, Punto(..),
        Relativo, Pezzo(..), Figura, renderFigura, Rendering)

import Graphics.Gloss.Data.Picture (Picture(..))
import Debug.Trace
import Data.Zip (Selector, moveSelector, filterDuplicates, labella)
import Data.Tree (drawTree, Tree)
import Data.List.Zipper
       (elimina, destra, sinistra, modifica, inserisci, valore, elementi,
        Zipper(..))
import Graphics.Gloss.Interface.Game
import Data.Graph (reachable)
import Data.Tree.Missing
       (backward, forward, Ricentratore, modifyTop)
import Interface.Register (register, catchRegister, CatchEvent, Movimenti, mkMovimenti)
import Control.Arrow (ArrowChoice(..))
import Control.Exception (assert)


data IFigura = IFigura
        {   ifigura :: Figura
        ,   iselectors :: forall b. [Selector Tree b]
        ,   iforward :: forall b . Ricentratore b
        ,   ibackward :: forall b . Ricentratore b
        }

ricentra :: Punto -> IFigura -> IFigura
ricentra l (IFigura ifig isels _ ibackw) = let
            ifig' = ibackw $ assolutizza ifig
            isels' = map (moveSelector ifig ibackw) isels
            ir = vicino l ifig'
            lifig = labella [0..] $ ifig'
            c = head $ snd (ir lifig)
            iforw =  forward c lifig
            ibackw' =  backward c lifig
            ifig'' = relativizza . iforw $ ifig'
            isels'' = map (moveSelector ifig' iforw) isels'
            in trace (drawTree $ fmap show . labella [0..] $  ifig'') $ IFigura ifig'' isels'' iforw ibackw'


-----------------------------   rendering ---------------------------------------------
renderIFigura re (IFigura ifig isels iforw _) = Pictures . renderFigura re'' $ ifig
    where
    re' = foldr (\ir re -> fst (ir re) $ (Color yellow .)) (iforw re) isels
    re'' = modifyTop (Color blue .) re'


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
        f (IFigura ifig ir iforw ibackw) = let
            ifig' = modifyTop g ifig
            g (Pezzo p o alpha) = Pezzo (p + l' - l) o alpha
            in IFigura ifig' ir iforw ibackw
        in modifica f z
registraR :: CatchEvent (Movimenti (Zipper IFigura))
registraR = register (Char 'r') $ \ l z l' -> let
        f (IFigura ifig ir iforw ibackw) = let
            ifig' = foldr (\(ir,alpha) -> ruotaScelto ir alpha) ifig (zip ir $ map iralpha ir)
            iralpha ir = let
                Pezzo q _ _ = head . snd $ ir (assolutizza ifig)
                alpha = atan2 y' x' - atan2 y x
                Punto (x,y) = l - q
                Punto (x',y') = l' - q
                in alpha
            in IFigura ifig' ir iforw ibackw
        in modifica f z

changeWorld :: CatchEvent (Zipper IFigura, Movimenti (Zipper IFigura))

changeWorld (EventKey (Char 'c') Down _ _ ) (z, mov)  = Just (inserisci id z, mov)
changeWorld (EventKey (Char 'd') Down _ _ ) (z, mov)  = Just (maybe z id $ elimina z, mov)
changeWorld (EventKey (MouseButton WheelUp) Up _ _ ) (z, mov)  = Just (destra z, mov)
changeWorld (EventKey (MouseButton WheelDown) Up _ _ ) (z, mov)  = Just (sinistra z, mov)
changeWorld (EventKey (Char 's') Down _ (Punto -> l')) (z, mov)  = Just (modifica f z, mov) where
    f (IFigura ifig ir iforw ibackw) = let
            ir' = vicino l' (assolutizza ifig)
            in IFigura ifig (filterDuplicates ifig (ir':ir)) iforw ibackw
changeWorld (EventKey (Char 'g') Down _ (Punto -> l)) (z, mov)  = Just (modifica (ricentra l) z, mov) where

changeWorld e z = catchRegister [registraT, registraR] e z


changeWorldG e w = maybe w id $ changeWorld e w



----------------------------- time -----------------------------------------------------
stepWorld :: Float -> a -> a
stepWorld = const id

type World = Zipper IFigura

run :: Rendering Picture -> World -> IO ()
run re world = gameInWindow "marionetta" (600,600) (0,0) white 100 (world,mkMovimenti) (renderWorldG re) changeWorldG stepWorld




