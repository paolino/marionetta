{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Movie where

import Prelude hiding (foldr, zipWith)
import Data.Foldable


import Math.Spline.BSpline
import Math.Spline.Knots (Knots)

import Data.Tree

import Data.Vector (fromList)

import Math 

import Model

import Data.List.Zipper

import Data.Tree.Missing

import Data.Zip

type Index = Int


spline :: Knots Float -> [Pezzo Assoluto] -> BSpline Punto
spline x = bSpline x . fromList . map originePezzo 


type Movie = [Tree (Pezzo Assoluto)]

splines :: Knots Float -> Movie -> Tree (BSpline Punto)
splines x (y:ys) = fmap (spline x) . foldr (zipWith (:)) (fmap return y) $ ys
splines x [] = error "empty movie to spline"

generaPasso :: Tree (Pezzo Assoluto) -> Tree (Pezzo Assoluto)  -> Fulcrum -> Tempo Normalizzato -> Figura 
generaPasso fp fa (Fulcrum s p) = let	
	(forw,_) = fromSelector fp s
	fp' = relativizza $ fp
	fa' = relativizza $ fa
	in interpolazione fp' fa'

data Fulcrum = Fulcrum 
	  	{	legato :: Selector Tree Label
		,	fulcrum :: Punto
		}

		
