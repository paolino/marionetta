-- | Some useful functions to work with Data.Tree.Tree
module Data.Tree.Missing (TreePath, recurseTreeAccum, replaceTreeNode, zipTreeWith, modifyNode, coords, pick) where

import Data.List (splitAt)
import Data.Tree
import Control.Monad.State (evalState, get , put, when)
import Control.Applicative
import Data.Traversable
import Data.Foldable
import Control.Arrow
------------------------------------------------------------
coords :: Eq a => a -> Tree a -> Maybe TreePath
coords s (Node s' ns) 
	| s == s' = Just []
	| otherwise = msum . zipWith (\n -> fmap (n:)) [0..] $ map (coords s) ns

pick :: TreePath -> Tree a -> Maybe a
pick [] (Node x _) = Just x
pick (n:ns) (Node x ts) = case drop n ts of
	[] -> Nothing
	(y:_) -> pick ns y


replaceNodeList :: Int -> [a] -> (a,a -> [a])
replaceNodeList n xs = case splitAt n xs of
   (_,[]) -> error "path to a non-existent node"
   (us,x:vs)  -> (x,\x -> us ++ [x] ++ vs)

type TreePath = [Int]

replaceTreeNode :: (Tree a -> Tree a) -> TreePath -> Tree a -> Tree a
replaceTreeNode f ns x = replaceNode' ns x where
   replaceNode' [] x = f x
   replaceNode' (n:ns) (Node a ts) = Node a . g $ replaceNode' ns t
      where (t,g) = replaceNodeList n ts

recurseTreeAccum :: b -> (b -> a -> (b,c)) -> Tree a -> Tree c
recurseTreeAccum x f n = recurse' x n where
  recurse' x (Node y ns) = let
    (x',z) = f x y
    ns' = map (recurse' x') ns
    in Node z ns'

modifyNode :: (a -> a) -> Tree a -> Tree a
modifyNode f (Node x ns) = Node (f x) ns

zipTreeWith :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipTreeWith f t1 t2 = evalState (traverse g t1) $ toList t2
  where
  g x  = do
    ys <- get
    when (null ys) $ error "zipping on trees with different shape"
    put $ tail ys
    return $ f x (head ys)


