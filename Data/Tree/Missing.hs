-- | Some useful functions to work with Data.Tree.Tree
module Data.Tree.Missing ( recurseTreeAccum, zipTreeWith, modifyPosition,inspectPosition, inspectTop, modifyTop) where
import Data.List (splitAt)
import Data.Tree
import Control.Monad.State (evalState, get , put, when)
import Control.Applicative
import Control.Arrow
------------------------------------------------------------




zipTreeWith f (Node x xs) (Node y ys) = Node (f x y) $ zipWith (zipTreeWith f) xs ys

inspectPosition :: (a -> Bool) -> Tree a -> Tree b -> [b]
inspectPosition j t tb = replace' (t,tb)  where
	replace' (Node x xs, Node y ys)
		| j x = [y]
                | otherwise = concatMap replace' $ zip xs ys

modifyPosition :: (a -> Bool) -> Tree a -> Tree b -> (b -> b) -> Tree b
modifyPosition j t tb f = replace' (t,tb)  where
	replace' (Node x xs, Node y ys)
		| j x = Node (f y) ys
                | otherwise = Node y . map replace' $ zip xs ys

recurseTreeAccum :: b -> (b -> a -> (b,c)) -> Tree a -> Tree c
recurseTreeAccum x f n = recurse' x n where
  recurse' x (Node y ns) = let
    (x',z) = f x y
    ns' = map (recurse' x') ns
    in Node z ns'

inspectTop (Node x _) = x
modifyTop f (Node x xs) = Node (f x) xs
