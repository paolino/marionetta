
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Controller where


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
import Model (ruotaScelto, vicino, Punto (..), Assoluto (..), Pezzo (..), assolutizza, relativizza)
import Interfaccia (IFigura (..))

data Movimenti a = Movimenti
    {   lastPunto :: Maybe Punto
    ,   movimenti :: Map Char (Movimento a)
    }

mkMovimenti = Movimenti Nothing empty

type Movimento a = Punto -> a -> Punto -> a

type CatchEvent a = Event -> a -> Maybe a

catchMevs :: CatchEvent (a, Movimenti a)
catchMevs (EventKey (MouseButton _) _ _ _) (x,Movimenti _ movs) =  Just  (x,Movimenti Nothing movs)
catchMevs (EventMotion (Punto -> p)) (x,Movimenti q movs) =
    Just (foldr (\f x -> f (fromMaybe p q) x p) x . elems $ movs, Movimenti (Just p)  movs)

catchMevs _ _  = Nothing

register :: Key -> Movimento a -> CatchEvent (a,Movimenti a)
register c@(Char z)  m (EventKey e Down _ (Punto -> p)) (x,Movimenti q movs)
    | c == e = Just  (m (fromMaybe p q) x p,  Movimenti (Just p) $ insert z m movs)
    | otherwise = Nothing
register c@(Char z) m (EventKey e Up _  _) (x,Movimenti p movs)
    | c == e = Just  (x,Movimenti Nothing $ delete z movs)
    | otherwise = Nothing

catchRegister :: [CatchEvent (a,Movimenti a)] -> CatchEvent (a, Movimenti a)
catchRegister regs ev (w,movs) = catchMevs' <|> catchRegs where
        catchMevs' = catchMevs ev (w,movs)
        catchRegs = msum . map (\r -> r ev (w,movs)) $ regs

routingPezzi :: Punto -> Routing (Pezzo Assoluto) -> Tree (Pezzo Assoluto) -> Tree (Pezzo Assoluto)
routingPezzi p r = snd . r (Pezzo p undefined undefined) (\(Pezzo c _ _) (Pezzo _ o alpha) -> Pezzo c o alpha)

rotazioneInOrigine :: Tree (Pezzo Assoluto) -> Tree (Pezzo Assoluto)
rotazioneInOrigine = modifyTop $ \(Pezzo _ o alpha) ->  Pezzo o o alpha

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

type World = (Zipper IFigura, Movimenti (Zipper IFigura))

catchMovimento :: [(Key,Movimento IFigura)] -> CatchEvent World
catchMovimento xs = catchRegister [register e (\p x q -> modifica (\i -> f p i q) x) | (e,f) <- xs] 

traslazione :: Movimento IFigura
traslazione  l (IFigura ifig ir iforw ibackw) l' = let
            ifig' = modifyTop g ifig
            g (Pezzo p o alpha) = Pezzo (p + l' - l) o alpha
            in IFigura ifig' ir iforw ibackw

rotazione :: Movimento IFigura
rotazione l (IFigura ifig ir iforw ibackw) l' = let
            ifig' = foldr (uncurry ruotaScelto) ifig (zip ir $ map iralpha ir)
            iralpha ir = let
                Pezzo q _ _ = head . snd $ ir (assolutizza ifig)
                alpha = atan2 y' x' - atan2 y x
                Punto (x,y) = l - q
                Punto (x',y') = l' - q
                in alpha
            in IFigura ifig' ir iforw ibackw

movimentoCentroTop :: Movimento IFigura
movimentoCentroTop l (IFigura ifig ir iforw ibackw) l' = IFigura ifig' ir iforw ibackw 
	where ifig' = relativizza . modifyTop (\(Pezzo _ o alpha) -> Pezzo l o alpha) . assolutizza $ ifig


catchEvents :: CatchEvent World 
catchEvents (EventKey (Char 'c') Down _ _ ) = Just . first (inserisci id)
catchEvents (EventKey (Char 'd') Down _ _ ) = Just . first (\z -> fromMaybe z $ elimina z)
catchEvents (EventKey (MouseButton WheelUp) Up _ _ ) = Just . first destra 
catchEvents (EventKey (MouseButton WheelDown) Up _ _ ) = Just . first sinistra
catchEvents (EventKey (Char 'g') Down _ (Punto -> l)) = Just . first (modifica $ ricentra l)
catchEvents (EventKey (Char 'z') Down _ _) = Just . first (modifica f) where
	f (IFigura ifig _ iforw ibackw) = IFigura ifig [] iforw ibackw
catchEvents (EventKey (Char 's') Down _ (Punto -> l)) = Just .  first (modifica f) where
	f (IFigura ifig ir iforw ibackw) = IFigura ifig (filterDuplicates ifig (ir':ir)) iforw ibackw where
		ir' = vicino l . assolutizza $ ifig
catchEvents e = catchMovimento [(Char 't', traslazione), (Char 'r' , rotazione), (Char 'x', movimentoCentroTop)] e 

controller :: Event -> World -> World 
controller e w = fromMaybe w $ catchEvents e w







