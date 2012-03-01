module Pattern where

import Control.Applicative
import Data.Fixed
import Data.List

data Event a = Event {onset :: Double, duration :: Maybe Double, value :: a}

instance Functor Event where
  fmap f e = e {value = (fmap f value e)}

instance (Show a) => Show (Event a) where
  show e = show $ value e

data Pattern a = Pattern {events :: [Event a], periodP :: Double}
--               | PatternCat [Pattern a]
               | PatternCombo [Pattern a]

data Signal a = Signal {func :: (Double -> a), periodS :: Double}

instance Functor Pattern where
  fmap f p@(Pattern {events = e}) = p {events = fmap (fmap f) e}
--  fmap f (PatternCat ps)   = PatternCat   $ fmap (fmap f) ps
  fmap f (PatternCombo ps) = PatternCombo $ fmap (fmap f) ps

instance Functor Signal where  
  fmap f s@(Signal _ _) = s {func = fmap f (func s)}
  
-- instance Applicative Pattern where
--  pure x = Pattern {events = [(0,x)], period = 1}
--  Pattern fs pf <*> Pattern xs px = Pattern (liftA2 (<*>) fs xs) (min pf px)

-- instance Applicative Pattern where
--   pure x = Pattern (pure (pure (pure x))) (Just 1)
--   Pattern fs pf <*> Pattern xs px = Pattern (liftA2 (zipCycleA2 (<*>)) fs xs) (lcd pf px)

instance (Show a) => Show (Pattern a) where
  show (Pattern _ 0) = ""
  show p = show $ events p

class Patternable p where
  pattern :: p a -> Pattern a

instance Patternable [] where
  pattern xs = Pattern r 1
    where 
      r = map (\x -> Event {onset = (fromIntegral x) / 
                                    (fromIntegral $ length xs),
                            duration = Nothing,
                            value = xs !! x
                           }
              ) [0 .. (length xs) - 1]


rev :: Pattern a -> Pattern a
rev = mapTime (1 -)

(<~) :: Pattern a -> Double -> Pattern a
p <~ d = mapTime (\x -> mod' (x - d) 1) p

(~>) :: Pattern a -> Double -> Pattern a
p ~> d = p <~ (0-d)


every :: Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
every 0 _ p = p
every n f p = cat $ (take (n-1) $ repeat p) ++ [f p]

cat :: [Pattern a] -> Pattern a
cat ps = Pattern (concatMap events ps') n
  where shrunk = map (\p -> mapTime (* ((periodP p) / n)) p) ps
        withOffsets = zip (0:(map (\p -> (periodP p) / n) shrunk)) shrunk
        ps' = map (\(o, p) -> mapTime (+ o) p) $ accumFst withOffsets
        n = (sum $ (map periodP) ps)

combine :: [Pattern a] -> Pattern a
combine = PatternCombo

accumOnsets :: [Event a] -> [Event a]
accumOnsets = scanl1 (\a b -> mapOnset (+ (onset a)) b)

mapOnset :: (Double -> Double) -> Event a -> Event a
mapOnset f e = e {onset = f $ onset e}

accumFst :: [(Double, a)] -> [(Double, a)]
accumFst = scanl1 (\a b -> mapFst (+ (fst a)) b)

mapTime :: (Double -> Double) -> Pattern a -> Pattern a
mapTime f p = p {events = map (mapOnset f) (events p)}

sinewave :: Int -> [Double]
sinewave n =  map (\x -> sin (step * fromIntegral x)) [0 .. n-1]
  where step = (pi * 2.0) / fromIntegral n

sinewave1 :: Int -> [Double]
sinewave1 = map ((/ 2) . (+ 1)) . sinewave

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

