module Pattern where

import Control.Applicative
import Data.Fixed
import Data.List

data Event a = Event {duration :: Maybe Double, value :: a}

instance Functor Event where
  fmap f e = e {value = (fmap f value e)}

instance (Show a) => Show (Event a) where
  show e = show $ value e

data Pattern a = Atom {event :: Event a, onset :: Double}
               | Cycle {patterns :: [Pattern a],
                        --density :: Double,
                        reps :: Double}
               | Combo {patterns :: [Pattern a]}

instance Functor Pattern where
  fmap f p@(Atom {event = e}) = p {event = fmap f e}
  fmap f p@(Cycle {patterns = ps}) = p {patterns = fmap (fmap f) ps}
  fmap f (Combo ps) = Combo $ fmap (fmap f) ps

instance (Show a) => Show (Pattern a) where
  show (Atom e o) = show e ++ "@" ++ show o
  show (Cycle ps r) =
    (show r) ++ " x (" ++ (intercalate " " (map show ps)) ++ ")"
  show (Combo ps) = intercalate ", " (map show ps)

type Signal a = (Double -> a)

--instance Functor Signal where
--  fmap f s = fmap f s

class Patternable p where
  pattern :: p a -> Pattern a

instance Patternable [] where
  pattern xs = Cycle r 1
    where
      r = map (\x -> Atom {onset = (fromIntegral x) /
                                   (fromIntegral $ length xs),
                           event = Event {
                             duration = Nothing,
                             value = xs !! x
                             }
                          }
              ) [0 .. (length xs) - 1]

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

sinewave :: Signal Double
sinewave = sin . (pi * 2 *)

sinewave1 :: Signal Double
sinewave1 = ((/ 2) . (+ 1)) . sinewave

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

modulateOnset :: (a -> Double -> Double) -> Signal a -> Pattern b -> Pattern b
modulateOnset f s p = mapOnset (\x -> f (s x) x) p

wobble :: Double -> Pattern a -> Pattern a
wobble d p = modulateOnset (+) (fmap (*d) sinewave) p

--flatten :: (Double, Double) -> Pattern a -> Pattern a
--flatten (startCycle, endCycle) p =



--modulate :: (a -> b -> c) -> Pattern a -> Signal b -> Pattern c
--modulate f p s = fmap (