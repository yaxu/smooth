module Pattern where

import Control.Applicative
import Data.Fixed
import Data.List
import Data.Maybe

data Pattern a = Atom {event :: a}
               | Arc {pattern  :: Pattern a,
                      onset    :: Double,
                      duration :: Maybe Double
                     }
               | Cycle {patterns :: [Pattern a]}



--data Pattern a = Atom a | Arc (Pattern a) (Double) (Maybe Double) | Cycle 

instance Functor Pattern where
  fmap f p@(Atom {event = a}) = p {event = f a}
  fmap f p@(Arc {pattern = p'}) = p {pattern = fmap f p'}
  fmap f p@(Cycle {patterns = ps}) = p {patterns = fmap (fmap f) ps}

instance (Show a) => Show (Pattern a) where
  show (Atom e) = show e
  show (Arc p o d) = concat [show p, "@", show o, "x", show d]
  show (Cycle ps) = "(" ++ (intercalate ", " (map show ps)) ++ ")"

type Signal a = (Double -> a)

--instance Functor Signal where
--  fmap f s = fmap f s

class Patternable p where
  toPattern :: p a -> Pattern a

instance Patternable [] where
  toPattern xs = Cycle ps
    where
      ps = map (\x -> Arc {pattern = Atom $ xs !! x,
                           onset = (fromIntegral x) /
                                   (fromIntegral $ length xs),
                           duration = Nothing
                          }
               ) [0 .. (length xs) - 1]

{-size :: Pattern a -> Double
size (Atom {})  = 1
size (Cycle {extent = e}) = e
size (Combo []) = 0
size (Combo ps) = maximum $ map size ps
-}


silence :: Pattern a
silence = Cycle []

mapAtom :: (Pattern a -> Pattern b) -> Pattern a -> Pattern b
mapAtom f p@(Atom {}) = f p
mapAtom f p@(Arc {pattern = p'}) = p {pattern = mapAtom f p'}
mapAtom f p@(Cycle {patterns = ps}) = p {patterns = fmap (mapAtom f) ps}

mapArc :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
mapArc f p@(Atom {}) = p
mapArc f p@(Arc {pattern = p'}) = f $ p {pattern = mapArc f p'}
mapArc f p@(Cycle {patterns = ps}) = p {patterns = fmap (mapArc f) ps}

{-
mapEvent :: (Event a -> Event b) -> Pattern a -> Pattern b
mapEvent f p = mapAtom (\p' -> p' {event = f (event p')}) p
-}

mapOnset :: (Double -> Double) -> Pattern a -> Pattern a
mapOnset f p = mapArc (\p' -> p' {onset = f $ onset p'}) p

rev :: Pattern a -> Pattern a
rev = mapOnset (1 -)

(<~) :: Double -> Pattern a -> Pattern a
d <~ p = mapOnset (\x -> mod' (x - d) 1) p

(~>) :: Double -> Pattern a -> Pattern a
d ~> p = (0-d) <~ p


-- assumes equal duration..

cat :: [Pattern a] -> Pattern a
cat ps = Cycle $ map a [0 .. (length ps) - 1]
  where l = length ps
        d = 1 / (fromIntegral l)
        a n = Arc {pattern = ps !! n,
                   onset = d * (fromIntegral n),
                   duration = Just d
                  }

every :: Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
every 0 _ p = p
every n f p = cat $ (take (n-1) $ repeat p) ++ [f p]


--cat :: [Pattern a] -> Pattern a
--cat ps = Cycle (concatMap events ps') n
--  where shrunk = map (\p -> mapTime (* ((periodP p) / n)) p) ps
--        withOffsets = zip (0:(map (\p -> (periodP p) / n) shrunk)) shrunk
--        ps' = map (\(o, p) -> mapTime (+ o) p) $ accumFst withOffsets
--        n = (sum $ (map periodP) ps)

combine :: [Pattern a] -> Pattern a
combine = Cycle

--accumOnsets :: [Event a] -> [Event a]
--accumOnsets = scanl1 (\a b -> mapOnset (+ (onset a)) b)

--accumFst :: [(Double, a)] -> [(Double, a)]
--accumFst = scanl1 (\a b -> mapFst (+ (fst a)) b)

sample :: Int -> Signal a -> Pattern a
sample n s = Cycle ps
  where 
    d = 1 / (fromIntegral n)
    ps = map (\x ->
               Arc {
                 pattern = Atom (s $ (fromIntegral x) * d),
                 onset = (fromIntegral x) * d,
                 duration = Just d
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

mapSnds :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnds = map . mapSnd

modulateOnset :: (a -> Double -> Double) -> Signal a -> Pattern b -> Pattern b
modulateOnset f s p = mapOnset (\x -> f (s x) x) p

wobble :: Double -> Pattern a -> Pattern a
wobble d p = modulateOnset (+) (fmap (*d) sinewave) p

flatten :: Pattern a -> [(Double, a)]
flatten (Atom e) = [(0, e)]
flatten Arc {pattern = p, onset = o, duration = d} = 
  squash o d $ flatten p
flatten (Cycle ps) = concatMap flatten ps

squash :: Double -> Maybe Double -> [(Double, a)] -> [(Double, a)]
squash o d es = mapFsts ((+ o) . (* (fromMaybe 1 d))) es

{-
accumFst :: [(Double, a)] -> [(Double, a)]
accumFst = scanl1 (\a b -> mapFst (+ (fst a)) b)
modulate :: (a -> b -> c) -> Pattern a -> Signal b -> Pattern c
modulate f p s = fmap (
-}

