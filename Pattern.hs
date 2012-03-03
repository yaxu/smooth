module Pattern where

import Control.Applicative
import Data.Fixed
import Data.List
import Data.Maybe

data Event a = Event {duration :: Maybe Double, value :: Maybe a}

instance Functor Event where
  fmap f e = e {value = (fmap (fmap f) value e)}

instance (Show a) => Show (Event a) where
  show e = show $ value e

data Pattern a = Atom {event :: Event a, onset :: Double}
               | Cycle {patterns :: [Pattern a],
                        extent :: Double}
               | Combo {patterns :: [Pattern a]}

instance Functor Pattern where
  fmap f p@(Atom {event = e}) = p {event = fmap f e}
  fmap f p@(Cycle {patterns = ps}) = p {patterns = fmap (fmap f) ps}
  fmap f (Combo ps) = Combo $ fmap (fmap f) ps

instance (Show a) => Show (Pattern a) where
  show (Atom e o) = show e ++ "@" ++ show o
  show (Cycle ps e) =
    (show e) ++ " x (" ++ (intercalate " " (map show ps)) ++ ")"
  show (Combo ps) = intercalate ", " (map show ps)

type Signal a = (Double -> a)

--instance Functor Signal where
--  fmap f s = fmap f s

class Patternable p where
  pattern :: p a -> Pattern a

instance Patternable [] where
  pattern xs = Cycle r (fromIntegral $ length xs)
    where
      r = map (\x -> Atom {onset = (fromIntegral x) /
                                   (fromIntegral $ length xs),
                           event = Event {
                             duration = Nothing,
                             value = Just $ xs !! x
                             }
                          }
              ) [0 .. (length xs) - 1]

size :: Pattern a -> Double
size (Atom {})  = 1
size (Cycle {extent = e}) = e
size (Combo []) = 0
size (Combo ps) = maximum $ map size ps

mapAtom :: (Pattern a -> Pattern b) -> Pattern a -> Pattern b
mapAtom f p@(Atom _ _) = f p
mapAtom f p@(Cycle {patterns = ps}) = p {patterns = fmap (mapAtom f) ps}
mapAtom f p@(Combo {patterns = ps}) = p {patterns = fmap (mapAtom f) ps}

mapEvent :: (Event a -> Event b) -> Pattern a -> Pattern b
mapEvent f p = mapAtom (\p' -> p' {event = f (event p')}) p

mapOnset :: (Double -> Double) -> Pattern a -> Pattern a
mapOnset f p = mapAtom (\p' -> p' {onset = f $ onset p'}) p

rev :: Pattern a -> Pattern a
rev = mapOnset (1 -)

(<~) :: Double -> Pattern a -> Pattern a
d <~ p = mapOnset (\x -> mod' (x - d) 1) p

(~>) :: Double -> Pattern a -> Pattern a
d ~> p = (0-d) <~ p

every :: Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
every 0 _ p = p
every n f p = cat $ (take (n-1) $ repeat p) ++ [f p]

cat :: [Pattern a] -> Pattern a
cat ps = Cycle ps 1

--cat :: [Pattern a] -> Pattern a
--cat ps = Cycle (concatMap events ps') n
--  where shrunk = map (\p -> mapTime (* ((periodP p) / n)) p) ps
--        withOffsets = zip (0:(map (\p -> (periodP p) / n) shrunk)) shrunk
--        ps' = map (\(o, p) -> mapTime (+ o) p) $ accumFst withOffsets
--        n = (sum $ (map periodP) ps)

combine :: [Pattern a] -> Pattern a
combine = Combo

--accumOnsets :: [Event a] -> [Event a]
--accumOnsets = scanl1 (\a b -> mapOnset (+ (onset a)) b)

--accumFst :: [(Double, a)] -> [(Double, a)]
--accumFst = scanl1 (\a b -> mapFst (+ (fst a)) b)

sample :: Int -> Signal a -> Pattern a
sample n s = Cycle ps 1
  where ps =
          map (\x ->
                Atom {
                  event = (Event {
                              duration = Nothing,
                              value = Just (s $ (fromIntegral x) / (fromIntegral n))
                              }
                          ),
                  onset = (fromIntegral x) / (fromIntegral n)
                  }
              )
          [0 .. (n - 1)]

sinewave :: Signal Double
sinewave = sin . (pi * 2 *)

sinewave1 :: Signal Double
sinewave1 = ((/ 2) . (+ 1)) . sinewave

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)

mapFsts :: (a -> b) -> [(a, c)] -> [(b, c)]
mapFsts = map . mapFst

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

modulateOnset :: (a -> Double -> Double) -> Signal a -> Pattern b -> Pattern b
modulateOnset f s p = mapOnset (\x -> f (s x) x) p

wobble :: Double -> Pattern a -> Pattern a
wobble d p = modulateOnset (+) (fmap (*d) sinewave) p

flatten :: (Double, Double) -> Pattern a -> [(Double, Event a)]
flatten (start, end) p@(Atom {onset = o, event = e})
  | and [o >= start, o < end] = [(o, e)]
  | otherwise = []
flatten r p@(Combo ps) = concatMap (flatten r) ps

{-flatten (start, end) p@(Cycle ps e)
  | end < start = []
  | otherwise = flat
  where cycles = map
                 (\offset -> (fromIntegral offset, p))
                 [floor start .. (ceiling end) - 1]
        flat = concatMap 
               (\(o, p') -> mapFsts (+ o) $ flatten (max (start - o) 0, min (end - o) 1) p')
               cycles
-}

flatten' (start, end) p@(Cycle {patterns = ps})
  | end < start = error "foo"
  | otherwise = flatEvents
    where totalSize :: Double
          totalSize = sum $ map size ps
          flatPs = map (flatten (0, 1)) ps
          positions = map (/ totalSize) $ scanl (+) 0 $ map size ps
          ranges = zip (zip (positions) (tail positions)) flatPs
          flatEvents = 
            concatMap (\((a, b), es) -> map (\(o, e) -> (a + (o * (b - a)), e)) es) ranges
          cycles :: [Double]
          cycles = map fromIntegral [floor start .. (ceiling end) - 1]
          flat = concatMap 
                 (\o -> mapFsts (+ o) $ flatten (max (start - o) 0, min (end - o) 1) p)
                 cycles
{-
        totalSize = sum $ map size ps
        positions = map (/ totalSize) $ scanl (+) 0 (map size ps)
        ranges = zip (zip (positions) (tail positions)) ps
        startCycle = mod' start 1
        duration = end - start
        deltaCycle = startCycle + duration
        endCycle = mod' end 1
        h = filter (\((s, e), p) -> s > startCycle && e < deltaCycle) ranges
        t | (startCycle + duration) < 1 = filter (\((s, e), p) -> e < endCycle) ranges
          | otherwise = []
-}

accumFst :: [(Double, a)] -> [(Double, a)]
accumFst = scanl1 (\a b -> mapFst (+ (fst a)) b)


--modulate :: (a -> b -> c) -> Pattern a -> Signal b -> Pattern c
--modulate f p s = fmap (