-----------------------------------------------------------------------------
--
-- Module      :  Linguaggio
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  paolo.veronelli@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Linguaggio (

) where

import Data.Tree (Tree)
import Model
       (assolutizza, relativizza, spostaFulcro, interpolazione, Assoluto, Punto(..),
        Pezzo(..), Pezzo, Relativo, Tempo(..), Normalizzato)
import Data.List (mapAccumL)
import Control.Arrow (Arrow(..))
import Data.Maybe (fromJust, isJust)
import Control.Applicative ((<$>), liftA2)




type Configurazione = Tree (Pezzo Relativo)

-- | I due passi possibili per l'evoluzione della marionetta. Cambio di configurazione con specificato il tempo in cui essa deve avvenire e cambio dell'indicizzazione dei pezzi, assegnando un punto che seleziona il nuovo pezzo principale come il più vicino a quel punto.
data Passo     = CambioConfigurazione (Tempo Relativo) Configurazione
               | CambioCima Punto deriving (Show,Read)

-- | Un evoluzione come insieme di passi
type Evoluzione = [Passo]


evoluzione :: Configurazione -> [Passo] -> Tempo Assoluto -> Configurazione
evoluzione = undefined

(.*.) :: Tempo Normalizzato -> Tempo Relativo -> Tempo Relativo
Tempo x .*. Tempo y = Tempo (x * y)
(.+.) :: Tempo Assoluto -> Tempo Relativo -> Tempo Assoluto
Tempo x .+. Tempo y = Tempo (x + y)

tempiAssoluti :: Configurazione -> [Passo] -> [(Tempo Assoluto , Tempo Normalizzato -> Configurazione)]
tempiAssoluti c0 = map (second fromJust) . filter (isJust . snd) . tail . map snd . scanl f (c0,(0,Just undefined)) where
    f (c, (t,_)) (CambioConfigurazione dt c') = (c',(t .+. dt, Just $ interpolazione c c'))
        where t' = t + dt
    f (c, (t,_)) (CambioCima p) = (catch $ relativizza <$> spostaFulcro p p (assolutizza c) ,(t, Nothing))
        where catch = maybe (error "fallimento nello spostare la cima") id
