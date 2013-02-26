{-# OPTIONS_GHC -XNoMonomorphismRestriction #-}

module Strategies where

import Pattern
import Dirt
import Data.Ratio
import Control.Applicative

echo n p = combine [p, n ~> p]
double f p = combine [p, f p]

-- every 4 (smash 4 [1, 2, 3]) $ sound "[odx sn/2 [~ odx] sn/3, [~ hh]*4]"

smash n xs p = cat $ map (\n -> slow n p') xs
  where p' = striate n p

brak = every 2 (((1%4) <~) . (\x -> cat [x, silence]))

-- samples "jvbass [~ latibro] [jvbass [latibro jvbass]]" ((1%2) <~ slow 6 "[1 6 8 7 3]")
samples p p' = sample <$> p <*> p'

spread f xs p = cat $ map (\x -> f x p) xs

spread' :: (a -> Sequence b -> Sequence c) -> Sequence a -> Sequence b -> Sequence c
spread' f timepat pat =
  Sequence $ \r -> concatMap (\(r', x) -> (range (f x pat) r')) (rs r)
  where rs r = mapFsts (mapSnd Just) $ range (filterOffsets timepat) r
