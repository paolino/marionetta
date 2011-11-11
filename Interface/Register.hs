-----------------------------------------------------------------------------
--
-- Module      :  Interface.Register
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
{-# LANGUAGE ViewPatterns #-}

module Interface.Register  where


import Graphics.Gloss.Interface.Game (Event(..), KeyState (..), Key (..) )
import Data.Maybe (Maybe)
import Graphics.Gloss.Interface.Game (Key, Modifiers)
import Data.Map (delete, insert, Map, empty)
import Model (Punto(..))
import Data.Foldable (msum, toList)
import Control.Applicative (Alternative ((<|>)))
import Debug.Trace (trace)


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
    Just (foldr (\f x -> f (maybe p id q) x p) x . toList $ movs, Movimenti (Just p)  movs)
catchMevs _ _  = Nothing

register :: Key -> Movimento a -> CatchEvent (Movimenti a)
register c@(Char z)  m (EventKey e Down _  _) (Movimenti p movs)
    | c == e = Just . Movimenti p $ insert z m movs
    | otherwise = Nothing
register c@(Char z) m (EventKey e Up _  _) (Movimenti p movs)
    | c == e = Just . Movimenti Nothing $ delete z movs
    | otherwise = Nothing

catchRegister :: [CatchEvent (Movimenti a)] -> CatchEvent (a, Movimenti a)
catchRegister regs ev (w,movs) = catchMevs' <|> catchRegs where
        catchMevs' = catchMevs ev (w,movs)
        catchRegs = (,) w `fmap` msum (map (\r -> r ev movs) regs)
