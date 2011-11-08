{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE ImpredicativeTypes #-}

module Interfaccia where


import Linguaggio
       (deserializza, Sequenza(..), renderFigura, Rendering,
        Serializzazione(..))
import Data.Tree.Missing (Ispettore)
import Graphics.Gloss
       (line, displayInWindow, Picture, white, gameInWindow)
import Model (Relativo, Pezzo(..))
import Graphics.Gloss.Interface.Game (Event)
import Graphics.Gloss.Data.Picture (Picture(..))
import Debug.Trace


data World = World
    {   persistente :: Serializzazione
    ,   volatile :: forall b . Maybe (Ispettore b)
    ,   rendereringWorld :: Rendering Picture
    }

renderWorld :: World -> Picture
renderWorld (World se Nothing re ) = let
    ps = concatMap (renderFigura re . partenza) ss
    ss = deserializza se
    in Pictures ([line [(-100,0),(100,0)], line [(0,100),(0,-100)]] ++ ps)

changeWorld :: Event -> World -> World
changeWorld = const id

stepWorld :: Float -> World -> World
stepWorld = const id


run :: World -> IO ()
--run world = gameInWindow "marionetta" (300,300) (0,0) white 100 world renderWorld changeWorld stepWorld
run world = displayInWindow "marionetta" (300,300) (0,0) white $ renderWorld world
