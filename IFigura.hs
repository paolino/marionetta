{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module IFigura where

import Data.Tree (Tree)
import Data.Tree.Missing (routingDumb, forward, backward,modifyTop, Routing, fromSelector, IRouting (..))
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
            (IRouting iforw , IRouting ibackw') = fromSelector ifig' ir 
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








