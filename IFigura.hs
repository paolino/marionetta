{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ImpredicativeTypes #-}

module IFigura where


import Control.Monad (msum)
import Control.Arrow (first)
import Control.Applicative ((<|>))
import Data.Map (Map, empty,elems,insert,delete )
import Data.Maybe (fromMaybe)
import Data.Tree (Tree)

import Graphics.Gloss.Interface.Game (Event (..), MouseButton (..), Key (..), KeyState (..))

import Data.List.Zipper (Zipper , inserisci, elimina, destra, sinistra, modifica)
import Data.Tree.Missing (routingDumb, forward, backward,modifyTop, Routing)
import Data.Zip (Selector, moveSelector, filterDuplicates, labella)
import Model (Figura, ruotaScelto, vicino, Punto (..), Pezzo (..), assolutizza, relativizza,rotazioneInOrigine, routingPezzi)

data IFigura = IFigura
        {   ifigura :: Figura
        ,   iselectors :: forall b. [Selector Tree b]
        ,   iforward :: forall b . Routing b
        ,   ibackward :: forall b . Routing b
        }


ricentra :: Punto -> IFigura -> IFigura
ricentra l (IFigura ifig isels _ ibackw ) = let
            ifig' = rotazioneInOrigine . routingPezzi undefined ibackw $ assolutizza ifig
            isels' = map (moveSelector ifig $ routingDumb ibackw) isels
            ir = vicino l ifig'
            lifig = labella [0..] ifig'
            c = head $ snd (ir lifig)
            iforw =  forward c lifig
            ibackw' =  backward c lifig
            ifig'' = relativizza . rotazioneInOrigine . routingPezzi undefined iforw $ ifig'
            isels'' = map (moveSelector ifig' $ routingDumb iforw) isels'
            in  IFigura ifig'' isels'' iforw ibackw'

type Movimento a = Punto -> Punto -> a -> a


traslazione :: Movimento IFigura
traslazione  l l' (IFigura ifig ir iforw ibackw) = let
            ifig' = modifyTop g ifig
            g (Pezzo p o alpha) = Pezzo (p + l' - l) o alpha
            in IFigura ifig' ir iforw ibackw

rotazione :: Movimento IFigura
rotazione l l' (IFigura ifig ir iforw ibackw) = let
            ifig' = foldr (uncurry ruotaScelto) ifig (zip ir $ map iralpha ir)
            iralpha ir = let
                Pezzo q _ _ = head . snd $ ir (assolutizza ifig)
                alpha = atan2 y' x' - atan2 y x
                Punto (x,y) = l - q
                Punto (x',y') = l' - q
                in alpha
            in IFigura ifig' ir iforw ibackw

movimentoCentroTop :: Movimento IFigura
movimentoCentroTop l l' (IFigura ifig ir iforw ibackw) = IFigura ifig' ir iforw ibackw 
	where ifig' = relativizza . modifyTop (\(Pezzo _ o alpha) -> Pezzo l o alpha) . assolutizza $ ifig


modificaSelettori l (IFigura ifig ir iforw ibackw) = IFigura ifig (filterDuplicates ifig (ir':ir)) iforw ibackw where
		ir' = vicino l . assolutizza $ ifig








