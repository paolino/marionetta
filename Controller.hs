{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Controller where


import Control.Monad (msum)
import Control.Arrow (first)
import Control.Applicative ((<|>))
import Data.Map (Map, empty,elems,insert,delete )
import Data.Maybe (fromMaybe)
import Data.Tree (Tree)


import Data.List.Zipper (mkZipper, Zipper , inserisci, elimina, destra, sinistra, modifica)
import Data.Tree.Missing (inspectTop , routingDumb, forward, backward,modifyTop, Routing)
import Data.Zip (Selector, moveSelector, filterDuplicates, labella)
import Model (Figura, ruotaScelto, vicino, Punto (..), Assoluto (..), Pezzo (..), assolutizza, relativizza)
import IFigura

data MoveEffect = Ruotando Punto | Traslando Punto | SpostandoCentro Punto | Niente

data World = World (Zipper IFigura) MoveEffect

mkWorld :: Figura -> World 
mkWorld fig = World 
	(mkZipper $ IFigura fig [] (forward (inspectTop fig) fig) (backward (inspectTop fig) fig))
	Niente

type Change = World -> World

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

catch :: Evento -> Change
catch Refresh (World z _) = World z Niente
catch (Puntatore p) (World z Niente) = World z Niente
catch (Puntatore p) (World z (Traslando q)) = World (modifica (traslazione q p) z) $ Traslando p 
catch (Puntatore p) (World z (Ruotando q)) = World (modifica (rotazione q p) z) $ Ruotando p 
catch (Puntatore p) (World z (SpostandoCentro q)) = World (modifica (movimentoCentroTop q p) z) $ SpostandoCentro p 
catch (Rotazione p Inizio) (World z _) = World z (Ruotando p)
catch (Rotazione p Fine) (World z (Ruotando _)) = World z Niente
catch (Rotazione p Fine) w = w
catch (Traslazione p Inizio) (World z _) = World z (Traslando p)
catch (Traslazione p Fine) (World z (Traslando _)) = World z Niente
catch (Traslazione p Fine) w = w
catch (SpostamentoCentro p Inizio) (World z _) = World z (SpostandoCentro p)
catch (SpostamentoCentro p Fine) (World z (SpostandoCentro _)) = World z Niente
catch (SpostamentoCentro p Fine) w = w
catch Cancella (World z m) = World (fromMaybe z $ elimina z) m
catch Clona (World z m) = World (inserisci id z) m
catch (Fuoco Destra) (World z m) = World (destra z) m
catch (Fuoco Sinistra) (World z m) = World (sinistra z) m
catch (Seleziona p) (World z m) = World (modifica (modificaSelettori p) z) m
catch Deseleziona (World z m) = World (modifica f z) m where
	f ifig = ifig {iselectors = []}
catch (Ricentra p) (World z m)  = World (modifica (ricentra p) z) m
catch Silent w = w





