{-# LANGUAGE Rank2Types #-}

-- | Some useful functions to work with Data.Tree.Tree
module Data.Tree.Missing ( recurseTreeAccum, zipTreeWith,  inspectTop, modifyTop, moveTop, Ispettore, ispettore) where
import Data.List (splitAt,inits,tails)
import Data.Tree
import Control.Monad
import Control.Applicative
import Control.Arrow
import Debug.Trace
import Data.Foldable (toList)
------------------------------------------------------------



zipTreeWith :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipTreeWith f (Node x xs) (Node y ys) = Node (f x y) $ zipWith (zipTreeWith f) xs ys

type Ispettore = forall b . (Tree b -> ((b -> b) -> Tree b, [b]))

ispettore  :: (a -> Bool) -> Tree a  ->Ispettore
ispettore t tr tr' = (flip  (fmap . ch)  &&&  map snd . filter (t . fst) . toList) $ zipTreeWith (,) tr tr' where
    ch f (x,y)    | t x  = f y
                     | otherwise = y

recurseTreeAccum :: b -> (b -> a -> (b,c)) -> Tree a -> Tree c
recurseTreeAccum x f n = recurse' x n where
  recurse' x (Node y ns) = let
    (x',z) = f x y
    ns' = map (recurse' x') ns
    in Node z ns'

inspectTop (Node x _) = x
modifyTop f (Node x xs) = Node (f x) xs

route cond = moveTo  where
	moveTo c n@(Node x ys)
		| cond x = Just [x]
		| null ys = Nothing
		| otherwise = fmap (x:) . msum $ map (moveTo c) ys
			


moveTop k cond x0 = move $ const id  where
	move c n@(Node x ys)
		| cond x = Just . Node (k x0 x) $ c x ys
		| null ys = Nothing
		| otherwise = msum $ zipWith move (map mkc yss) ys where
			yss = zipWith (++) (inits ys) . tail . tails $ ys
			mkc ys x' ys' = Node (k x' x) (c x ys) : ys'
