-----------------------------------------------------------------------------
--
-- Module      :  Data.Tree.Missing
-- Copyright   :  Paolo Veronelli
-- License     :  BSD3
--
-- Maintainer  :  paolo.veronelli@gmail.com
-- Stability   :  Unstable
-- Portability :  Portable
--
-- | Some operators for Tree structures.

-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types #-}

module Data.Tree.Missing (inspectTop, modifyTop, recurseTreeAccum, backward, forward, 
    Routing, routingDumb, fromSelector, topSelector, IRouting (..)) where

import Prelude hiding (zipWith)
import Control.Monad (msum)
import Data.List (splitAt,inits,tails)
import Data.Tree (Tree (Node))

import Data.Zip (Zip (..), labella, Selector , Label, mkSelector)



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
insertAt n x xs = let (as,bs) = splitAt n xs in as ++ x : bs
replaceAt n x xs = let (as,_:bs) = splitAt n xs in as ++ x:bs


forward :: (Eq a) => a -> Tree a -> Routing b
forward y tr x0 f tr'@(Node x _) =   (,) x . fmap snd . maybe (error "missing element in ricentratore") id . move (const id) . zipWith (,) tr $ tr'
        where
            move c n@(Node (x,x2) ys)
                | x == y = Just $ Node (x,f x0 x2) $ c x2 ys
                | null ys = Nothing
                | otherwise = msum $ zipWith move (map mkc [0..]) ys
                    where  mkc n x0 ys' = (Node (x, f x0 x2) . c x2 $ dropAt n ys):ys'


backward :: ( Eq a) => a -> Tree a -> Routing b
backward y tr x0 f =    maybe (error "missing element in ricentratore") id . move  Nothing $ tr
        where
            reverting Nothing mh (Node x (y:ys)) = maybe (x,Node x (y:ys)) (\h -> h (x,y) $ Node x ys) mh
            reverting (Just (n, (x1,Node x (y:ys)))) (Just h) (Node xy ysy) = let ys' = insertAt n (Node (f x xy) ysy) ys in h (x1,y) $ Node x ys'
            reverting (Just (n, (x1,Node x ys))) Nothing (Node xy ysy) = (x1,Node (f x0 x) $ insertAt n (Node (f x xy) ysy) ys)
            move h n@(Node x ys)
                | x == y = Just $ reverting Nothing h
                | null ys = Nothing
                | otherwise = msum $ zipWith move [Just $ \ y -> reverting (Just (n,y)) h | n <- [0..]] ys

type Routing b =  b -> (b -> b -> b) -> Tree b -> (b , Tree b)

routingDumb :: Routing b -> Tree b -> Tree b
routingDumb r = snd . r undefined (const id)

topSelector :: Tree a -> Selector Tree b
topSelector t = mkSelector (==0) $ labella t

newtype IRouting = IRouting (forall c . Routing c)
fromSelector :: Tree a -> Selector Tree Label -> (IRouting , IRouting)
fromSelector ifig ir = let
    lifig = labella ifig
    r = head $ snd (ir lifig)
        in (IRouting $ forward r lifig, IRouting $ backward r lifig)


