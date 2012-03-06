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
               | Signal {at :: Double -> Pattern a}

-- make Silence constructor to use instead of Cycle [] ?

joinPattern :: Pattern (Pattern a) -> Pattern a
joinPattern = mapAtom (\(Atom x) -> x)

instance Monad Pattern where
  return = Atom
  m >>= f = joinPattern (fmap f m)

instance Applicative Pattern where
  pure = Atom

  Atom f <*> xs = mapAtom (fmap f) xs
  fs <*> (Atom x) = mapAtom (\(Atom f) -> Atom $ f x) fs

  (Cycle fs) <*> xs = Cycle $ map (<*> xs) fs
  fs <*> (Cycle xs) = Cycle $ map (fs <*>) xs

  fs@(Arc {onset = o}) <*> s@(Signal {}) = fs <*> (at s o)
  fs@(Arc {}) <*> xs@(Arc {}) | isIn fs xs = fs {pattern = (pattern fs) <*> (pattern xs)}
                              | otherwise = Cycle []

  fs@(Signal {}) <*> xs = Signal $ (<*> xs) . (at fs)
  fs <*> xs@(Signal {}) = Signal $ (fs <*>) . (at xs)

    --where s n = mapAtom (\x -> mapAtom (\f -> Atom $ (event f) (event x)) (at fs n)) xs

isIn :: Pattern a -> Pattern b -> Bool
isIn (Arc {onset = o1}) (Arc {onset = o2, duration = (Just d2)})
  = o1 >= o2 && o1 < (o2 + d2)
isIn (Arc {onset = o1}) (Arc {onset = o2, duration = Nothing})
  = o1 == o2
isIn _ _ = False -- only makes sense for Arcs

--data Pattern a = Atom a | Arc (Pattern a) (Double) (Maybe Double) | Cycle

instance Functor Pattern where
  fmap f p@(Atom {event = a}) = p {event = f a}
  fmap f p@(Arc {pattern = p'}) = p {pattern = fmap f p'}
  fmap f p@(Cycle {patterns = ps}) = p {patterns = fmap (fmap f) ps}
  fmap f p@(Signal _) = p {at = (fmap f) . (at p)}

instance (Show a) => Show (Pattern a) where
  show (Atom e) = concat ["(Atom ", show e, ")\n"]
  show (Arc p o d) = concat ["(Arc ", show p, "@", show o, "x", show d, ")\n"]
  show (Cycle ps) = "(cycle " ++ (intercalate ", " (map show ps)) ++ ")\n"
  show (Signal s) = "*signal*"

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
mapAtom f p@(Signal _) = p {at = fmap (mapAtom f) (at p)}

filterP :: (Pattern a -> Bool) -> Pattern a -> Pattern a
filterP f p@(Atom {}) | f p = p
                      | otherwise = Cycle []
filterP f p@(Cycle ps) | f p = p {patterns = map (filterP f) ps}
                       | otherwise = Cycle []
filterP f p@(Arc {}) | f p = p {pattern = filterP f (pattern p)}
                     | otherwise = Cycle []
filterP f p@(Signal {}) | f p = p {at = (filterP f) . (at p)}


mapArc :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
mapArc f p@(Atom {}) = p
mapArc f p@(Arc {pattern = p'}) = f $ p {pattern = mapArc f p'}
mapArc f p@(Cycle {patterns = ps}) = p {patterns = fmap (mapArc f) ps}
mapArc f p@(Signal _) = p {at = fmap (mapArc f) (at p)}


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

sinewave :: Pattern Double
sinewave = Signal {at = f}
  where f x = Arc {pattern = Atom $ (sin . (pi * 2 *)) x,
                   onset = mod' x 1,
                   duration = Nothing
                   }

sinewave1 :: Pattern Double
sinewave1 = fmap ((/ 2) . (+ 1))  sinewave

{-
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

modulateOnset :: (a -> Double -> Double) -> Signal a -> Pattern b -> Pattern b
modulateOnset f s p = mapOnset (\x -> f (s x) x) p

wobble :: Double -> Pattern a -> Pattern a
wobble d p = modulateOnset (+) (fmap (*d) sinewave) p
-}
mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)

mapFsts :: (a -> b) -> [(a, c)] -> [(b, c)]
mapFsts = map . mapFst

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

mapSnds :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnds = map . mapSnd



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

