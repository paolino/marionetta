-----------------------------------------------------------------------------
--
-- Module      :  Linguaggio
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  paolo.veronelli@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Linguaggio  where

import Data.Tree (Tree)
import Model
       (Angolo, assolutizza, relativizza, interpolazione, Assoluto,
        Punto(..), Pezzo(..), Pezzo, Relativo, Tempo(..), (.-.), (.+.),
        (./.), Normalizzato, polare)
import Data.List (mapAccumL)
import Control.Arrow (Arrow(..))
import Data.Maybe (catMaybes, fromJust, isJust)
import Control.Applicative ((<$>), liftA2)
import Control.Monad (liftM2)
import Data.Tree.Missing
       (zipTreeWith, ricentratore, Ricentratore, labella)
import Data.Foldable (toList)

import Debug.Trace



type Figura = Tree (Pezzo Relativo)
type Nome = Int

data Passo     = Passo
    {   nuovaFigura :: Figura
    ,   durataPasso :: Tempo Relativo
    ,   fulcroPasso :: Nome
    ,   centroFulcro :: Punto
    }   deriving (Show,Read)

data Serializzazione = Serializzazione
    {   figuraIniziale :: Figura
    ,   passi :: [Passo]
    } deriving (Show, Read)


data Sequenza b = Sequenza
    {   partenza :: Figura
    ,   finale  :: Figura
    ,   centratore :: Tree b -> Tree b
    ,   durataSequenza :: Tempo Relativo
    }

deserializza :: Serializzazione -> [Sequenza b]
deserializza s@(Serializzazione f0 ps) =  ss where
    ss = zipWith f (f0 : map nuovaFigura ps) ps where
    rif = labella [0..] f0
    f f0 (Passo f1 t n c) = Sequenza (r' f0) (r' f1) (r undefined (const id)) t
        where       r = ricentratore n rif :: Ricentratore b
                    r' = relativizza . r (Pezzo c undefined) gp . assolutizza
    gp :: Pezzo Assoluto -> Pezzo Assoluto -> Pezzo Assoluto
    gp (Pezzo c _) (Pezzo _ o) = Pezzo c o


type Renderer b = ((Punto, Angolo), Punto) -> b

type Rendering b = Tree (Renderer b)

renderFigura :: Rendering b -> Figura -> [b]
renderFigura r x = trace (show x) . toList . zipTreeWith f r . assolutizza $ x where
    f g p@(Pezzo c o) = g $ (polare $ p,c)


renderSequenza :: Rendering b -> Sequenza (Renderer b) -> Tempo Assoluto -> Tempo Assoluto -> [b]
renderSequenza re (Sequenza f0 f1 r dt) t0 t = renderFigura (r re) . interpolazione f0 f1 $ (t .-. t0) ./. dt


