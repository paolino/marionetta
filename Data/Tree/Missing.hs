
{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}

-- | Some useful functions to work with Data.Tree.Tree
module Data.Tree.Missing  where
import Prelude hiding (mapM, zipWith)
import Data.List (splitAt,inits,tails)
import Data.Tree
import Control.Monad hiding (mapM)
import Control.Applicative
import Control.Arrow
import Debug.Trace
import Data.Foldable (toList)
import Data.Traversable (mapAccumL, mapM)
import Control.Monad.State.Lazy (evalState)
import Control.Monad.State.Class (MonadState(..))
import Data.Zip
------------------------------------------------------------



instance Zip Tree where
    zipWith f (Node x xs) (Node y ys) =  Node (f x y) $ zipWith (zipWith f) xs ys

recurseTreeAccum :: b -> (b -> a -> (b,c)) -> Tree a -> Tree c
recurseTreeAccum x f n = recurse' x n where
  recurse' x (Node y ns) = let
    (x',z) = f x y
    ns' = map (recurse' x') ns
    in Node z ns'

inspectTop (Node x _) = x
modifyTop f (Node x xs) = Node (f x) xs


dropAt n xs = let (as,_:bs) = splitAt n xs in as ++ bs
insertAt (n,x) xs = let (as,bs) = splitAt n xs in as ++ x : bs

type Ricentratore b =  Tree b -> Tree b

forward :: (Show a , Eq a) => a -> Tree a -> Ricentratore b
forward y tr tr' =   fmap snd . maybe (error $ "missing element in ricentratore, forw: " ++ drawTree (fmap show tr)) id . move id . zipWith (,) tr $ tr'
        where
            move c n@(Node (x,x2) ys)
                | x == y = Just . Node (x,x2) $ c ys
                | null ys = Nothing
                | otherwise = msum $ zipWith move (map mkc [0..]) ys
                    where  mkc n = (:) (Node (x, x2) . c $ dropAt n ys)

search :: (Eq a) => a -> Tree a -> Maybe a
search y (Node x ys) | x == y = Just x
                     | null ys = Nothing
                     | otherwise = msum $ map (search y) ys

backward :: ( Eq a) => a -> Tree a -> Ricentratore b
backward y tr =    maybe (error "missing element in ricentratore") id . move Nothing $ tr
        where
            reverting Nothing mh (Node x (y:ys)) = maybe (Node x (y:ys)) (\h -> h y $ Node x ys) mh
            reverting (Just (n, Node x (y:ys))) (Just h) y' = let ys' = insertAt (n,y') ys in h y (Node x ys')
            reverting (Just (n, Node x ys)) Nothing y = Node x $ insertAt (n,y) ys
            move h n@(Node x ys)
                | x == y = Just $ reverting Nothing h
                | null ys = Nothing
                | otherwise = msum $ zipWith move [Just $ \ y -> reverting (Just (n,y)) h | n <- [0..]] ys

niceTree = putStrLn . drawTree . fmap show

checkFB n s = mapM_ niceTree [s, forward n s s, backward n s (forward n s s)]

