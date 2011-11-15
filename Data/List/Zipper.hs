module Data.List.Zipper where

data Zipper a = Zipper [a] [a]

sinistra z@(Zipper [] _) = z
sinistra (Zipper (x:xs) ys) = Zipper xs (x:ys)

destra z@(Zipper _ [x]) = z
destra (Zipper xs (y:ys)) = Zipper (y:xs) ys

mkZipper x = Zipper [] [x]
valore (Zipper _ (x:_)) = x

inserisci f (Zipper xs (y:ys)) = Zipper (y:xs) ((f y):ys)

elimina (Zipper [] [y]) = Nothing 
elimina (Zipper (x:xs) [y]) = Just (Zipper xs [x])
elimina (Zipper xs (y:ys)) = Just (Zipper xs ys)

modifica f (Zipper xs (y:ys)) = Zipper xs (f y : ys)
elementi (Zipper xs ys) = xs ++ ys



