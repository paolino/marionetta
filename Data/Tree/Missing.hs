

-- | Some useful functions to work with Data.Tree.Tree
module Data.Tree.Missing  where
import Prelude hiding (mapM)
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
------------------------------------------------------------



zipTreeWith :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipTreeWith f (Node x xs) (Node y ys) = Node (f x y) $ zipWith (zipTreeWith f) xs ys

type Ispettore b = Tree b -> ((b -> b) -> Tree b, [b])

ispettore  :: (a -> Bool) -> Tree a  -> Ispettore b
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

labella :: [a] -> Tree b -> Tree a
labella xs = snd . mapAccumL (\(x:xs) _ -> (xs,x)) xs

type Ricentratore b =  b -> (b -> b -> b) -> Tree b -> Tree b

ricentratore :: Eq a => a -> Tree a -> Ricentratore b
ricentratore y tr x0 k  = fmap snd . catch .  move (const id) . zipTreeWith (,) tr  where
        catch = maybe (error "missing element in ricentratore") id
	move c n@(Node (x,x2) ys)
		| x == y = Just . Node (x, k x0 x2) $ c x2 ys
		| null ys = Nothing
		| otherwise = msum $ zipWith move (map mkc yss) ys where
			yss = zipWith (++) (inits ys) . tail . tails $ ys
			mkc ys x' ys' = Node (x, k x' x2) (c x2 ys) : ys'


