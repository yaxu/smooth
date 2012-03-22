module Pattern where

import Control.Applicative
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Ratio
import Debug.Trace

data Pattern a = Atom {event :: a}
             | Cycle {patterns :: [Pattern a]}
             | Composition {cf :: (Rational, Rational) -> Pattern a}
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

  (Cycle fs) <*> xs = Cycle $ map (<*> xs) fs
  fs <*> (Cycle xs) = Cycle $ map (fs <*>) xs
  
  Atom f <*> xs = f <$> xs
  fs <*> (Atom x) = (\f -> f x) <$> fs

  fs@(Arc {reps = 1}) <*> xs@(Arc {reps = 1}) = down
     where down | isIn fs xs = fs {pattern = (pattern fs) <*> (pattern xs)}
                | otherwise = Silence

  fs@(Arc {}) <*> xs@(Arc {}) = Composition f
    where f r = a <*> b
               where a | reps fs == 1 = fs
                       | otherwise = flatten r fs
                     b | reps xs == 1 = xs 
                       | otherwise = flatten r xs

  fs@(Arc {onset = o}) <*> s@(Signal {}) = applySignal (0, 1) fs (at s)

  (Composition f) <*> xs = Composition f'
     where f' o = (f o) <*> xs

  fs <*> (Composition f) = Composition f'
      where f' o = fs <*> (f o)

  fs@(Signal {}) <*> xs = Signal $ (<*> xs) . (at fs)
  fs <*> xs@(Signal {}) = Signal $ (fs <*>) . (at xs)
  _ <*> Silence = Silence
  Silence <*> _ = Silence

_ %% 0 = 0
a %% b = a % b

_ // 0 = 0
a // b = a / b

applySignal :: (Rational, Rational) -> Pattern (a -> b) -> (Rational -> Pattern a) -> Pattern b

applySignal (o, s) p@(Cycle fs) sig
  = Cycle $ map (\f -> applySignal (o, s) f sig) fs

applySignal (o, s) p@(Arc {pattern = p', onset = o', scale = s'}) sig
  = p {pattern = applySignal (o'', s'') p' sig}
  where o'' = o + (o' * s)
        s'' = (o + ((o' + s') * s)) - o''

applySignal (o, s) fs sig
  = fs <*> (sig o)

instance Monad Pattern where
  return = pure
  m >>= f = joinPattern (fmap f m)

    --where s n = mapAtom (\x -> mapAtom (\f -> Atom $ (event f) (event x)) (at fs n)) xs

isIn :: Pattern a -> Pattern b -> Bool
isIn (Arc {onset = o1}) (Arc {onset = o2, scale = s})
  = (o1 >= o2 && o1 < (o2 + s)) 
    -- || (r2 == 0 && o1 == o2)
isIn _ _ = False

instance Functor Pattern where
  fmap f p@(Atom {event = a}) = p {event = f a}
  fmap f p@(Arc {pattern = p'}) = p {pattern = fmap f p'}
  fmap f p@(Cycle {patterns = ps}) = p {patterns = fmap (fmap f) ps}
  fmap f p@(Signal _) = p {at = (fmap f) . (at p)}
  fmap f p@(Composition f') = Composition $ \o -> fmap f (f' o)

  fmap _ Silence = Silence

instance (Show a) => Show (Pattern a) where
  show (Atom e) = show e
  show (Arc p o d r) = concat [" [", show p, "@(", show o, ")x(", show d, ")-(", show r, ")]"]
  show (Cycle ps) = " (" ++ (intercalate ", " (map show ps)) ++ ") "
  show (Composition _) = "*composition*"
  show (Signal s) = "*signal*"
  show Silence = "~"


class Patternable p where
  toPattern :: p a -> Pattern a

instance Patternable [] where
  toPattern [] = Silence
  toPattern xs = Cycle ps
    where
      ps = map (\x -> Arc {pattern = Atom $ xs !! x,
                           onset = (fromIntegral x) %%
                                   (fromIntegral l),
                           scale = 1 %% (fromIntegral l),
                           reps = 1
                          }
               ) [0 .. l - 1]
      l = length xs

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
mapAtom f (Composition f') = Composition (\o -> mapAtom f (f' o))
mapAtom _ Silence = Silence

filterP :: (Pattern a -> Bool) -> Pattern a -> Pattern a
filterP f p@(Atom {}) | f p = p
                      | otherwise = Silence
filterP f p@(Cycle ps) | f p = p {patterns = map (filterP f) ps}
                       | otherwise = Silence
filterP f p@(Arc {}) | f p = p {pattern = filterP f (pattern p)}
                     | otherwise = Silence
filterP f p@(Signal {}) | f p = p {at = (filterP f) . (at p)}
                        | otherwise = Silence
filterP f p@(Composition f') | f p = Composition (\o -> filterP f (f' o))
                        | otherwise = Silence
filterP _ Silence = Silence

mapArc :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
mapArc f p@(Atom {}) = p
mapArc f p@(Arc {pattern = p'}) = f $ p {pattern = mapArc f p'}
mapArc f p@(Cycle {patterns = ps}) = p {patterns = fmap (mapArc f) ps}
mapArc f p@(Signal _) = p {at = fmap (mapArc f) (at p)}
mapArc f (Composition f') = Composition (\o -> mapArc f (f' o))

mapArc _ Silence = Silence


{-
mapEvent :: (Event a -> Event b) -> Pattern a -> Pattern b
mapEvent f p = mapAtom (\p' -> p' {event = f (event p')}) p
-}

mapOnset :: (Rational -> Rational) -> Pattern a -> Pattern a
mapOnset f p = mapArc (\p' -> p' {onset = f $ onset p'}) p

rev :: Pattern a -> Pattern a
rev = mapOnset (\x -> x + (((ceiling x)%1) - x))

(<~) :: Rational -> Pattern a -> Pattern a
d <~ p = Arc p (mod' (0 - d) 1) 1 1

(~>) :: Rational -> Pattern a -> Pattern a
d ~> p = (0-d) <~ p


-- assumes equal scale..

cat :: [Pattern a] -> Pattern a
cat [] = Silence
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
every n f p = slow (fromIntegral n %1) $ cat $ (take (n-1) $ repeat p) ++ [f p]


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


triwave1 :: Pattern Double
triwave1 = Signal {at = f}
  where f x = Atom $ mod' (fromRational x) 1

triwave :: Pattern Double
triwave = Signal {at = f}
  where f x = fmap ((subtract 1) . (*2)) triwave1


squarewave1 :: Pattern Double
squarewave1 = Signal {at = f}
  where f x = Atom $ fromIntegral $ floor $ (mod' (fromRational x) 1) * 2

squarewave :: Pattern Double
squarewave = fmap ((subtract 1) . (* 2)) squarewave1

discretise :: Int -> Pattern a -> Pattern a
discretise n s = Cycle ps
  where
    d = 1 %% (fromIntegral n)
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

flat :: (Rational, Rational) -> Pattern a -> [((Rational, Rational), a)]

flat (a, b) (Silence) = []
flat (a, b) (Atom e) = foo -- trace ("\n" ++ show e ++ ": " ++ show a ++ "/" ++ show b ++ "=" ++ show (map fst foo)) $ foo
  where foo = map (\x -> ((x%1,(x+1)%1),e)) [ceiling a .. (ceiling b) - 1]
flat (a, b) (Composition f) = flat (a,b) $ f (a,b)
  where explode (x, y) = ((x * (b - a)) + a, (y * (b - a)) + a)
flat (a, b) (Cycle ps) = concatMap (flat (a, b)) ps

-- TODO - this is a bit of a hack for rotation
flat (a, b) p@(Arc {pattern = p', onset = o', scale = 1, reps = 1})
  = mapFsts (\(x, y) -> (x + o',y + o')) $ flat (a-o',b-o') p'
  
flat (a, b) p@(Arc {pattern = p', onset = o', scale = s', reps = r'})
  | a >= b = []
  | otherwise = (mapFsts squash (flat (a'', b'') p')) ++ rest
  where start = (floor a) % 1 -- start of loop
        next = start + 1      -- next loop
        b' = min b next       -- limit of present recursion along range
        innerStart = (r'*start) -- inner loop start
        innerNext = (r'*(start + 1))
        a'' = min innerNext $ innerStart + (max 0 (((a - start) - o') / s'))
        b'' = min innerNext $ innerStart + (max 0 (((b - start) - o') / s'))
--        a'' = min innerNext $ innerStart + (max 0 (((a - start) - o') / s'))
--        b'' = min innerNext $ innerStart + (max 0 (((b - start) - o') / s'))
        rest = flat (next, b) p
        squash (x,y) = (start + o' + ((x - innerStart) * s'),
                        start + o' + ((y - innerStart) * s')
                       )
                       
        
flat r p@(Signal _) = flat r $ discretise 64 p
        
{-

flat (o, s) Arc {pattern = p, onset = a', scale = s', reps = r} 
  | isIn = squash a' s' $ flat (max a'' 0, min s'' 1) p
  | otherwise = []
  where a = o
        b = a+s
        b' = a'+s'
        ia = max a a'
        ib = min b b'
        is = ib - ia
        a'' = (ia - a') / s'
        b'' = (ib - a') / s'
        s'' = b'' - a''
        isIn = a'' < 1 && b'' > 0 && a'' < b''
        isIn' = tr $ isIn
        tr = trace $ intercalate ", " [show a, show b, show a', show b', show isIn]    
  
-}
--flat (o, s) arc@(Arc {pattern = p, onset = o', scale = s', reps = r})
--  = flat (o, s) (arc {reps = 1, scale = s' / r, onset = o' + (mod' o r)})
--  where 
        

isWithin :: Rational -> Rational -> Rational ->  Rational -> Bool
isWithin a b a' b' = or [a' >= a && a' < b,
                         b' > a && b' <= b,
                         a' <= a && b' >= b
                        ]


flat' :: (Rational, Rational) -> Pattern a -> [(Double, a)]
flat' r p = mapFsts (\(x,y) -> fromRational $ (x - (fst r)) / (snd r - fst r)) (flat r p)

slow :: Rational -> Pattern a -> Pattern a
slow x p = Arc p 0 x (1/x)

unit :: Pattern a -> Bool
unit p = reps p == (1 / scale p)

flatten :: (Rational, Rational) -> Pattern a -> Pattern a
flatten (x, y) p = Cycle $ map (\(o, s, e) -> (Arc (Atom e) o s 1)) xs
  where xs = map norm $ flat (x,y) p
        norm ((x', y'), e) = ((x' - x) / d, (y' - x')/d, e)
        d = y - x



--squash :: Rational -> Rational -> [(Rational, a)] -> [(Rational, a)]
--squash o s es = mapFsts ((+ o) . (* s)) es

run len = toPattern [0 .. len-1]
scan n = cat $ map run [1 .. n]

