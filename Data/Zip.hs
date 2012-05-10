-----------------------------------------------------------------------------
--
-- Module      :  Data.Zip
-- Copyright   :  Paolo Veronelli
-- License     :  BSD3
--
-- Maintainer  :  paolo.veronelli@gmail.com
-- Stability   :  Unstable
-- Portability :  Portable
--
-- | Zip class for structures supporting zipWith operation. 
-- Selector operator to point to indexed elements in same shape structures.

-----------------------------------------------------------------------------

{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}

module Data.Zip  where


import Prelude hiding (zipWith)

import qualified Prelude (zipWith)
import Control.Arrow ((&&&))
import Data.Foldable (Foldable, toList)
import Data.Traversable (mapAccumL, Traversable)

class Zip t where
    zipWith :: (a -> b -> c) -> t a -> t b -> t c

instance Zip [] where
    zipWith = Prelude.zipWith

type Selector t b = t b -> ((b -> b) -> t b, [b])

mkSelector  :: (Functor t, Foldable t ,Zip t) => (a -> Bool) -> t a  -> Selector  t b
mkSelector t tr tr' = (flip  (fmap . ch)  &&&  map snd . filter (t . fst) . toList) $ zipWith (,) tr tr' where
    ch f (x,y)    | t x  = f y
                  | otherwise = y

type Label = Int 

labella :: Traversable t => t b -> t Label
labella = snd . mapAccumL (\(x:xs) _ -> (xs,x)) [0..]

moveSelector :: (Functor t, Foldable t, Traversable t, Zip t)
    => t a -> (forall a . t a -> t a) -> Selector t Bool  -> Selector t b
moveSelector tr r s = mkSelector id . r $ fst (s . fmap (const False) $ tr) (const True)


filterDuplicates :: forall t a b . (Functor t, Foldable t, Traversable t, Zip t)
    => t a -> (forall b . [Selector t b])  -> [Selector t b]
filterDuplicates tr ss = let
    ss' = zip [1..] ss
    tr' = fmap (const []) tr
    tr'' = map head . filter ((==  1) . length) .  toList $ foldr (\(i,s) tr -> fst (s tr) $ (i:) ) tr' ss'

    in  map snd . filter (flip elem tr'' . fst) $ ss'

