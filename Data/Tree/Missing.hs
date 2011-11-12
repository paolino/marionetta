
{-# LANGUAGE ScopedTypeVariables #-}

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
    zipWith f (Node x xs) (Node y ys) = Node (f x y) $ zipWith (zipWith f) xs ys

recurseTreeAccum :: b -> (b -> a -> (b,c)) -> Tree a -> Tree c
recurseTreeAccum x f n = recurse' x n where
  recurse' x (Node y ns) = let
    (x',z) = f x y
    ns' = map (recurse' x') ns
    in Node z ns'

inspectTop (Node x _) = x
modifyTop f (Node x xs) = Node (f x) xs

labella :: [a] -> Tree b -> Tree a
labella xs = snd . mapAccumL (\(x:xs) _ -> (xs,x)) xs


dropAt n xs = let (as,_:bs) = splitAt n xs in as ++ bs
insertAt (n,x) xs = let (as,bs) = splitAt n xs in as ++ x : bs

type Ricentratore b =  b -> (b -> b -> b) -> Tree b -> Tree b

forward :: Eq a => a -> Tree a -> Ricentratore b
forward y tr x0 k =  fmap snd . maybe (error "missing element in ricentratore") id . move (const id) . zipWith (,) tr
        where
            move c n@(Node (x,x2) ys)
                | x == y = Just . Node (x, k x0 x2) $ c x2 ys
                | null ys = Nothing
                | otherwise = msum $ zipWith move (map mkc [0..]) ys
                    where  mkc n x' = (:) (Node (x, k x' x2) . c x2 $ dropAt n ys)

backward :: Eq a => a -> Tree a -> Ricentratore b
backward y tr =    maybe (error "missing element in ricentratore") id . move (\(Node x ys) _ _ y -> Node x (y:ys)) $ tr
        where
            reverting my h x0 k (Node x ys) = let y:ys' = maybe id insertAt my ys
               in h (Node (k x0 x) ys') x k y
            move h n@(Node x ys)
                | x == y = Just $ reverting Nothing h
                | null ys = Nothing
                | otherwise = msum $ zipWith move [\ y -> reverting (Just (n,y)) h | n <- [0..]] ys

