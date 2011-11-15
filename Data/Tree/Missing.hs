
{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}

-- | Some useful functions to work with Data.Tree.Tree
module Data.Tree.Missing  where
import Prelude hiding (mapM, zipWith)
import Data.List (splitAt,inits,tails)
import Data.Tree
import Data.Either
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
insertAt n x xs = let (as,bs) = splitAt n xs in as ++ x : bs
replaceAt n x xs = let (as,_:bs) = splitAt n xs in as ++ x:bs

type Ricentratore b =  Tree b -> Tree b

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

routing :: forall a b . Eq a => a -> Tree a -> Routing b
routing xt tr x0 f = second (fmap snd) . either (error "endpoint not found in routing") id . search . zipWith (,) tr  where
    search (Node (x,x2) ys)
        | xt == x = Right(x2,Node (x,f x0 x2) ys)
        | null ys = Left (Node (x,x2) ys)
        | otherwise = let
            ys' = map search ys
            in case rights ys' of
                [] -> Left (Node (x,x2) ys)
                [(x0,_)] -> Right (x2,Node (x,f x0 x2) $ map (either id snd) ys')
                _ -> error "multiple endpoints in routing"




niceTree = putStrLn . drawTree . fmap show
{-
checkFB n s = mapM_ niceTree [s, forward n s s, backward n s (forward n s s)]
-}

