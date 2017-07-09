{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module Controller (Evento (..), catchEvento, mkWorld, World (..), Lasso (..), Verso (..), updateTime) where


import Data.Maybe (fromMaybe)
import Control.Arrow (first, second) 

import Data.List.Zipper (mkZipper, Zipper , inserisci, elimina, destra, sinistra, modifica)
import Data.Tree.Missing (inspectTop , forward, backward, topSelector)
import Model (Figura,  Punto(..), vicino, assolutizza, Tempo (..), Normalizzato)
import IFigura (IFigura(IFigura), ifigura , traslazione, rotazione, movimentoCentroTop, modificaSelettori, ricentra, iselectors)
import Movie

data MoveEffect = Ruotando Punto | Traslando Punto | SpostandoCentro Punto | SpostandoFulcrum Punto| Niente

data World = World (Tempo Normalizzato) (Zipper (IFigura,Fulcrum)) MoveEffect 
mkWorld :: Figura -> World 
mkWorld fig = World 
    (Tempo 0)
    (mkZipper $ (IFigura fig [] (forward (inspectTop fig) fig) (backward (inspectTop fig) fig),  Fulcrum (topSelector fig) $ Punto (0,0)))
    Niente

data Lasso = Inizio | Fine
data Verso = Destra | Sinistra

data Evento where
    Refresh ::  Evento 
    Puntatore :: Punto -> Evento 
    Rotazione :: Punto -> Lasso -> Evento 
    Traslazione :: Punto -> Lasso -> Evento 
    SpostamentoCentro :: Punto -> Lasso -> Evento 
    Cancella :: Evento 
    Clona :: Evento 
    Fuoco :: Verso -> Evento 
    Ricentra :: Punto -> Evento 
    Seleziona :: Punto -> Evento 
    Deseleziona :: Evento
    Silent :: Evento
    SpostamentoFulcrum :: Punto -> Lasso -> Evento
    RicentraFulcrum :: Punto -> Evento

modificaIFigura  f = modifica $ first f
modificaFulcrum f = modifica $ second f

catchEvento :: Evento -> World -> World
catchEvento Refresh (World t z _) = World t z Niente
catchEvento (Puntatore p) (World t z Niente) = World t z Niente
catchEvento (Puntatore p) (World t z (Traslando q)) = World t (modificaIFigura  (traslazione q p) z) $ Traslando p 
catchEvento (Puntatore p) (World t z (Ruotando q)) = World t (modificaIFigura  (rotazione q p) z) $ Ruotando p 
catchEvento (Puntatore p) (World t z (SpostandoCentro q)) = World t (modificaIFigura  (movimentoCentroTop q p) z) $ SpostandoCentro p 
catchEvento (Puntatore p) (World t z (SpostandoFulcrum q)) = World t (modificaFulcrum  (\ful -> ful {fulcrum = q}) z) $ SpostandoFulcrum p 
catchEvento (Rotazione p Inizio) (World t z _) = World t z (Ruotando p)
catchEvento (Rotazione p Fine) (World t z (Ruotando _)) = World t z Niente
catchEvento (Rotazione p Fine) w = w
catchEvento (Traslazione p Inizio) (World t z _) = World t z (Traslando p)
catchEvento (Traslazione p Fine) (World t z (Traslando _)) = World t z Niente
catchEvento (Traslazione p Fine) w = w
catchEvento (SpostamentoCentro p Inizio) (World t z _) = World t z (SpostandoCentro p)
catchEvento (SpostamentoCentro p Fine) (World t z (SpostandoCentro _)) = World t z Niente
catchEvento (SpostamentoCentro p Fine) w = w
catchEvento Cancella (World t z m) = World t (fromMaybe z $ elimina z) m
catchEvento Clona (World t z m) = World t (inserisci id z) m
catchEvento (Fuoco Destra) (World t z m) = World t (destra z) m
catchEvento (Fuoco Sinistra) (World t z m) = World t (sinistra z) m
catchEvento (Seleziona p) (World t z m) = World t (modificaIFigura  (modificaSelettori p) z) m
catchEvento Deseleziona (World t z m) = World t (modificaIFigura  f z) m where
    f ifig = ifig {iselectors = []}
catchEvento (Ricentra p) (World t z m)  = World t (modificaIFigura  (ricentra p) z) m
catchEvento Silent w = w
catchEvento (SpostamentoFulcrum p Inizio) (World t z _) = World t  z (SpostandoFulcrum p)
catchEvento (SpostamentoFulcrum p Fine) (World t z (SpostandoFulcrum _)) = World t z Niente
catchEvento (SpostamentoFulcrum p Fine) w = w
catchEvento (RicentraFulcrum p) (World t z m)  = World t (modifica f z) m where
    f (ifi, Fulcrum _ q) = (ifi, Fulcrum (vicino p (assolutizza $ ifigura ifi)) q)

updateTime :: Float -> World -> World
updateTime ((/3) -> t) (World (tempo -> t') z m) = World (Tempo t'') z m where
    t'' = if t + t' > 1 then 0 else t + t'
