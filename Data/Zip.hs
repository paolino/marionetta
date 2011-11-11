-----------------------------------------------------------------------------
--
-- Module      :  Data.Zip
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

module Data.Zip  where


import Prelude hiding (zipWith)

import qualified Prelude (zipWith)
import Control.Arrow (Arrow(..))
import Data.Foldable (Foldable, toList)


class Zip t where
    zipWith :: (a -> b -> c) -> t a -> t b -> t c

instance Zip [] where
    zipWith = Prelude.zipWith

type Selector t b = t b -> ((b -> b) -> t b, [b])

mkSelector  :: (Functor t, Foldable t ,Zip t) => (a -> Bool) -> t a  -> Selector  t b
mkSelector t tr tr' = (flip  (fmap . ch)  &&&  map snd . filter (t . fst) . toList) $ zipWith (,) tr tr' where
    ch f (x,y)    | t x  = f y
                  | otherwise = y

