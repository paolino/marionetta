
-----------------------------------------------------------------------------
--
-- Module      :  Data.List.Zipper
-- Copyright   :  Paolo Veronelli
-- License     :  BSD3
--
-- Maintainer  :  paolo.veronelli@gmail.com
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Zipper structure on list
--
-----------------------------------------------------------------------------



module Data.List.Zipper where

data Zipper a = Zipper [a] [a] 
instance Functor Zipper where
    fmap f (Zipper xs ys) = Zipper (fmap f xs) (fmap f ys)

sinistra z@(Zipper [] _) = z
sinistra (Zipper (x:xs) ys) = Zipper xs (x:ys)

destra z@(Zipper _ [x]) = z
destra (Zipper xs (y:ys)) = Zipper (y:xs) ys

isLast (Zipper [] _) = True
isLast _ = False

mkZipper x = Zipper [] [x]
valore (Zipper _ (x:_)) = x

inserisci f (Zipper xs (y:ys)) = Zipper (y:xs) ((f y):ys)

elimina (Zipper [] [y]) = Nothing 
elimina (Zipper (x:xs) [y]) = Just (Zipper xs [x])
elimina (Zipper xs (y:ys)) = Just (Zipper xs ys)

modifica f (Zipper xs (y:ys)) = Zipper xs (f y : ys)
elementi (Zipper xs ys) = reverse xs ++ ys



