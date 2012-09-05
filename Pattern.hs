module Pattern where

import Control.Applicative
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Ratio
import Debug.Trace

type T = Rational
type Range = (T, T)
type Event a = (Range, a)

--data Seq a = Seq {events :: [Event a]}

data Pattern a = Pattern {arc :: Range -> [Event a]}
               | Signal {at :: T -> [a]}

--instance (Show a) => Show (Seq a) where
--  show (Seq es) = show es

silence = Pattern $ const $ []

atom :: a -> Pattern a
atom x = Pattern $ \(s, d) -> map (\t -> ((fromIntegral t, fromIntegral t + 1), x)) [floor s .. (floor (s + d)) - 1]

--instance Functor Seq where
--  fmap f = Seq . mapSnds f . events

instance Functor Pattern where
  fmap f (Pattern a) = Pattern $ \r -> fmap (mapSnd f) $ a r
  fmap f (Signal a) = Signal $ \t -> fmap f (a t)

--instance Applicative Seq where
--  pure x = Seq [((0,1), x)]
--  (Seq fs) <*> (Seq xs) = Seq xs'
--    where xs' = concatMap (\f -> map (\(t, x) -> (t, (snd f) x)) $ filter (startsIn f) xs) fs

--startsIn :: Event a -> Event b -> Bool
--startsIn (t, _) (t', _) = (fst t') >= (fst t) && (fst t' <= (fst t + snd t))

instance Applicative Pattern where
  pure x = Signal $ const [x]
  (Pattern fs) <*> (Pattern xs) = 
    Pattern $ \r -> concatMap 
                    (\(t, f) -> map
                                (mapSnd f)
                                (filter (\(t', _) -> t == t') (xs r))
                    )
                    (fs r)
  
  (Signal fs) <*> (Signal xs) = Signal $ \t -> (fs t) <*> (xs t)
  
  (Signal fs) <*> (Pattern xs) = 
    Signal $ \t -> concatMap (\(_, x) -> map (\f -> f x) (fs t)) (xs (t,0))
--  Pattern $ \r -> concatMap (\(t, x) -> map (\f -> (t, f x)) (fs t)) (xs r)

  (Pattern fs) <*> (Signal xs) = 
    Pattern $ \r -> concatMap (\((o,d), f) -> map (\x -> ((o,d), f x)) (xs o)) (fs r)

{-
rep :: a -> Pattern a
rep x = Pattern $ \(s, d) -> Seq (map (\n -> ((fromIntegral n, 1), x)) [(ceiling s) .. (ceiling $ s+d) - 1])
-}

flatten :: (Rational, Rational) -> [Pattern a] -> [Event a]
flatten t ((Signal _):ps) = flatten t ps -- ignore signals
flatten (start, d) ps | d <= 0 = []
                      | otherwise =
  es ++ (flatten (segStop, d-(segStop-start)) ps)
  where l = length ps
        loopStart = (floor start) % 1
        segStart = fromIntegral (floor $ start * (fromIntegral l)) % (fromIntegral l)
        segD = 1 % (fromIntegral l)
        segStop = segStart + segD :: Rational
        patTime t = loopStart + ((t - segStart) * (fromIntegral l))
        patStart = patTime start
        patStop = min (patTime (start + d)) (loopStart + 1)
        patD = patStop - patStart
        patN = mod (floor $ start * (fromIntegral l)) l
        p = ps !! patN
        es = mapFsts scale $ (arc p) (patStart, patD)
        scale (sStart, sD)  = (((sStart - loopStart) * segD) + segStart,
                               segD
                               )

--        info = "\n" ++ concatMap (\(a, b) -> a ++ ": " ++ show b) things
--        things = [("start", start), (" d", d), (" segStart", segStart), (" segD", segD)]

-- ignores signals - should return a signal if any are signals?  via a fold..
cat :: [Pattern a] -> Pattern a
cat [] = silence
cat ps = Pattern $ \r -> flatten r ps

-- What about signals?
combine :: [Pattern a] -> Pattern a
combine ps = Pattern $ \r -> concatMap (\p -> (arc p) r) ps

patToOnsets :: Range -> Pattern a -> [Event a]
patToOnsets _ (Signal _) = [] --map (\x -> (t, x)) (a t)
patToOnsets r (Pattern a) = a r

filterEvents :: (Event a -> Bool) -> Pattern a -> Pattern a
filterEvents f (Pattern a) = Pattern $ \r -> filter f $ a r

-- Filter out events that start before range
filterOffsets :: Pattern a -> Pattern a
filterOffsets p@(Signal _) = p
filterOffsets p@(Pattern _) = filterEvents ((>= 0) . fst . fst) p

patToRelOnsets :: Range -> Pattern a -> [(Double, a)]
patToRelOnsets _ (Signal _) = []
patToRelOnsets (s, d) p = mapFsts (fromRational . (/ d) . (subtract s) . fst) $ patToOnsets (s, d) (filterOffsets p)

mapEvents :: (Event a -> Event a) -> Pattern a -> Pattern a
mapEvents f (Pattern a) = Pattern $ \r -> map f (a r)
mapEvents f (Signal a) = Signal $ \t -> map (\x -> snd $ f ((t,0), x)) (a t)

-- Maps time of events from an unmapped time range..  
-- Generally not what you want..

mapEventRange :: (Rational -> Rational) -> Pattern a -> Pattern a
mapEventRange f p = mapEvents (mapFst f') p
  where f' (s, d) = (f s, (f (s + d)) - (f s))

mapOnset :: (Rational -> Rational) -> Pattern a -> Pattern a
mapOnset f (Signal a) = Signal $ \t -> a (f t)
mapOnset f (Pattern a) = Pattern $ \(s, d) -> a (f s, d)

-- Function applied to both onset (start) and offset (start plus duration)
mapRange :: (Rational -> Rational) -> Pattern a -> Pattern a
mapRange f p@(Pattern a) = Pattern $ \(s, d) -> a (f s, (f (s + d)) - (f s))
mapRange f p = mapOnset f p

(<~) :: Rational -> Pattern a -> Pattern a
(<~) t p = mapEventRange (+ t) $ mapRange (subtract t) p

(~>) :: Rational -> Pattern a -> Pattern a
(~>) t p = mapEventRange (subtract t) $ mapRange (+ t) p

slow :: Rational -> Pattern a -> Pattern a
slow r p = mapEventRange (* r) $ mapRange (/ r) p

density :: Rational -> Pattern a -> Pattern a
density r p = mapEventRange (/ r) $ mapRange (* r) p

every :: Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
every 0 _ p = p
every n f p = slow (fromIntegral n %1) $ cat $ (take (n-1) $ repeat p) ++ [f p]

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)

mapFsts :: (a -> b) -> [(a, c)] -> [(b, c)]
mapFsts = map . mapFst

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

mapSnds :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnds = fmap . mapSnd
