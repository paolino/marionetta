module Render.Gloss where

import Graphics.Gloss
import Data.Tree (Tree (Node))
import Data.Tree.Missing (zipTreeWith)
import Data.Foldable (toList)
import Model

type Tempo = Float

renderSemiretta :: Tree (Semiretta -> Picture) -> Tree Semiretta ->  Picture
renderSemiretta tsp ts = Pictures . toList $ zipTreeWith (flip ($)) ts tsp

renderFilm  :: Tree (Semiretta -> Picture) ->  [Tree Semiretta] -> Tempo -> Tempo -> Picture
renderFilm tp ts tmax = let
	l = length ts
	dt = tmax / (fromIntegral l)
	tsr = map (renderSemiretta tp) ts
	in \t -> tsr !! (floor (t/dt) `mod` l)


