module Pattern where

import Control.Applicative
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Ratio

data Pattern a = Atom {event :: a}
             | Cycle {patterns :: [Pattern a]}
             | Signal {at :: Rational -> Pattern a}
             | Silence
             | Arc {pattern :: Pattern a,
                    onset :: Rational,
                    scale :: Rational,
                    reps :: Rational
                   }

joinPattern :: Pattern (Pattern a) -> Pattern a
joinPattern = mapAtom (\(Atom x) -> x)

instance Applicative Pattern where
  pure = Atom

  Atom f <*> xs = f <$> xs
  fs <*> (Atom x) = (\f -> f x) <$> fs

  (Cycle fs) <*> xs = Cycle $ map (<*> xs) fs
  fs <*> (Cycle xs) = Cycle $ map (fs <*>) xs

  fs@(Arc {}) <*> xs@(Arc {}) | isIn fs xs = fs {pattern = (pattern fs) <*> (pattern xs)}
                              | otherwise = Silence

--  fs@(Arc {onset = o}) <*> s@(Signal {}) = fs <*> (at s o)
  fs@(Arc {onset = o}) <*> s@(Signal {}) = applySignal (0, 1) fs (at s)

  fs@(Signal {}) <*> xs = Signal $ (<*> xs) . (at fs)
  fs <*> xs@(Signal {}) = Signal $ (fs <*>) . (at xs)
  _ <*> Silence = Silence
  Silence <*> _ = Silence

applySignal :: (Rational, Rational) -> Pattern (a -> b) -> (Rational -> Pattern a) -> Pattern b

applySignal (o, s) p@(Cycle fs) sig
  = Cycle $ map (\f -> applySignal (o, s) f sig) fs

applySignal (o, s) p@(Arc {pattern = p', onset = o', scale = s'}) sig
  = p {pattern = applySignal (o'', s'') p' sig}
  where o'' = o + (o' * s)
        s'' = o + ((o' + s') * s)

applySignal (o, s) fs sig
  = fs <*> (sig o)

instance Monad Pattern where
  return = pure
  m >>= f = joinPattern (fmap f m)

    --where s n = mapAtom (\x -> mapAtom (\f -> Atom $ (event f) (event x)) (at fs n)) xs

isIn :: Pattern a -> Pattern b -> Bool
isIn (Arc {onset = o1}) (Arc {onset = o2, reps = r2})
  = (o1 >= o2 && o1 < (o2 + r2)) 
    -- || (r2 == 0 && o1 == o2)
isIn _ _ = False

instance Functor Pattern where
  fmap f p@(Atom {event = a}) = p {event = f a}
  fmap f p@(Arc {pattern = p'}) = p {pattern = fmap f p'}
  fmap f p@(Cycle {patterns = ps}) = p {patterns = fmap (fmap f) ps}
  fmap f p@(Signal _) = p {at = (fmap f) . (at p)}
  fmap _ Silence = Silence

instance (Show a) => Show (Pattern a) where
  show (Atom e) = show e
  show (Arc p o d r) = concat ["[", show p, "@(", show o, ")x(", show d, ")]"]
  show (Cycle ps) = "(" ++ (intercalate ", " (map show ps)) ++ ")"
  show (Signal s) = "*signal*"
  show Silence = "~"

class Patternable p where
  toPattern :: p a -> Pattern a

instance Patternable [] where
  toPattern xs = Cycle ps
    where
<<<<<<< HEAD
      n = length xs
      ps = zipWith mkArc xs [0..]
      mkArc x i = Arc (Atom x) ((fromIntegral i) / (fromIntegral n)) Nothing
=======
      ps = map (\x -> Arc {pattern = Atom $ xs !! x,
                           onset = (fromIntegral x) %
                                   (fromIntegral l),
                           scale = 1 % (fromIntegral l),
                           reps = 1
                          }
               ) [0 .. l - 1]
      l = length xs
>>>>>>> b19758be4822b9ce98811259887619554c8717c9

{-size :: Pattern a -> Double
size (Atom {})  = 1
size (Cycle {extent = e}) = e
size (Combo []) = 0
size (Combo ps) = maximum $ map size ps
-}


silence :: Pattern a
silence = Silence

mapAtom :: (Pattern a -> Pattern b) -> Pattern a -> Pattern b
mapAtom f p@(Atom {}) = f p
mapAtom f p@(Arc {pattern = p'}) = p {pattern = mapAtom f p'}
mapAtom f p@(Cycle {patterns = ps}) = p {patterns = fmap (mapAtom f) ps}
mapAtom f p@(Signal _) = p {at = fmap (mapAtom f) (at p)}
mapAtom _ Silence = Silence

filterP :: (Pattern a -> Bool) -> Pattern a -> Pattern a
filterP f p@(Atom {}) | f p = p
                      | otherwise = Silence
filterP f p@(Cycle ps) | f p = p {patterns = map (filterP f) ps}
                       | otherwise = Silence
filterP f p@(Arc {}) | f p = p {pattern = filterP f (pattern p)}
                     | otherwise = Silence
filterP f p@(Signal {}) | f p = p {at = (filterP f) . (at p)}
filterP _ Silence = Silence

mapArc :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
mapArc f p@(Atom {}) = p
mapArc f p@(Arc {pattern = p'}) = f $ p {pattern = mapArc f p'}
mapArc f p@(Cycle {patterns = ps}) = p {patterns = fmap (mapArc f) ps}
mapArc f p@(Signal _) = p {at = fmap (mapArc f) (at p)}
mapArc _ Silence = Silence


{-
mapEvent :: (Event a -> Event b) -> Pattern a -> Pattern b
mapEvent f p = mapAtom (\p' -> p' {event = f (event p')}) p
-}

mapOnset :: (Rational -> Rational) -> Pattern a -> Pattern a
mapOnset f p = mapArc (\p' -> p' {onset = f $ onset p'}) p

rev :: Pattern a -> Pattern a
rev = mapOnset (1 -)

(<~) :: Rational -> Pattern a -> Pattern a
d <~ p = mapOnset (\x -> mod' (x - d) 1) p

(~>) :: Rational -> Pattern a -> Pattern a
d ~> p = (0-d) <~ p


-- assumes equal scale..

cat :: [Pattern a] -> Pattern a
cat ps = Cycle $ map a [0 .. (length ps) - 1]
  where l = length ps
        s = 1 % (fromIntegral l)
        a n = Arc {pattern = ps !! n,
                   onset = s * (fromIntegral n),
                   scale = s,
                   reps = 1
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
  where f x = Atom $ (sin . (pi * 2 *)) (fromRational x)

sinewave1 :: Pattern Double
sinewave1 = fmap ((/ 2) . (+ 1))  sinewave

sample :: Int -> Pattern a -> Pattern a
sample n s = Cycle ps
  where
    d = 1 % (fromIntegral n)
    ps = map (\x ->
               Arc {
                 pattern = (at s $ (fromIntegral x) * d),
                 onset = (fromIntegral x) * d,
                 scale = d,
                 reps = 1
                 }
             )
         [0 .. (n - 1)]

{-
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

flatten' :: Pattern a -> [(Double, a)]
flatten' p = mapFsts (fromRational) (flatten p)

flatten :: Pattern a -> [(Rational, a)]
flatten (Atom e) = [(0, e)]
flatten Arc {pattern = p, onset = o, scale = s, reps = r} =
  squash o s $ flatten p
flatten (Cycle ps) = concatMap flatten ps
flatten Silence = []

flat :: (Rational, Rational) -> Pattern a -> [(Rational, a)]
flat (o, d) (Silence) = []
flat (o, d) (Atom e) = [(0, e)]
flat (a, b) (Cycle ps) = concatMap (flat (a, b)) ps
flat (a, b) Arc {pattern = p, onset = o, scale = s, reps = r} 
  | isWithin a b a' b' = squash o s $ flat (0 - ((a'-a)/s'), 1 + ((b-b')/s')) p
--  | isWithin a b a' b' = squash o s $ flat (0 - ((a'-a)/s'), 1 + ((b-b')/s)) p
  | otherwise = []
  where s' = b - a
        a' = o
        b' = o + s

isWithin a b a' b' = or [a' >= a && a' < b,
                         b' > a && b' <= b,
                         a' <= a && b' >= b
                        ]


flat' :: (Rational, Rational) -> Pattern a -> [(Double, a)]
flat' r p = mapFsts (fromRational) (flat r p)


squash :: Rational -> Rational -> [(Rational, a)] -> [(Rational, a)]
squash o s es = mapFsts ((+ o) . (* s)) es


