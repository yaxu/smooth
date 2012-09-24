module Pattern where

import Control.Applicative
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Ratio
import Debug.Trace

type Range = (Rational, Rational)
type Event a = (Range, a)

data Pattern a = Sequence {arc :: (Rational, Maybe Rational) -> [Event a]}
               | Signal {at :: Rational -> [a]}

instance (Show a) => Show (Pattern a) where
  show p@(Sequence _) = show $ arc p (0, Just 1)
  show p@(Signal _) = "~signal~"

silence = Sequence $ const []

atom :: a -> Pattern a
atom x = Sequence f
  where f (s, Nothing) = [((fromIntegral (floor s), 1), x)]
        f (s, Just d) = map 
                        (\t -> ((fromIntegral t, 1), x)) 
                        [floor s .. (ceiling (s + d)) - 1]

instance Functor Pattern where
  fmap f (Sequence a) = Sequence $  fmap (fmap (mapSnd f)) a
  fmap f (Signal a) = Signal $ fmap (fmap f) a

instance Applicative Pattern where
  pure x = Signal $ const [x]
  (Sequence fs) <*> (Sequence xs) = 
    Sequence $ \r -> concatMap
                     (\((o,d),x) -> map
                                    (\(r', f) -> (r', f x))
                                    (
                                      filter
                                      (\((o',d'),_) -> (o' >= o) && (o' < (o+d)))
                                      (fs r)
                                    )
                     )
                     (xs r)
  (Signal fs) <*> (Signal xs) = Signal $ \t -> (fs t) <*> (xs t)
  (Signal fs) <*> px@(Sequence _) = 
    Signal $ \t -> concatMap (\(_, x) -> map (\f -> f x) (fs t)) (arc px (t,Nothing))
  (Sequence fs) <*> (Signal xs) = 
    Sequence $ \r -> concatMap (\((o,d), f) -> 
                                map (\x -> ((o,d), f x)) (xs o)) (fs r)

{-
at' :: Pattern a -> Rational -> [Event a]
at' p@(Sequence _) t = filter (\((t', _), _) -> t >= t') $ arc p (t, Nothing)
at' p@(Signal _) t = undefined
-}

cat :: [Pattern a] -> Pattern a
cat ps = combine $ map (squash l) (zip [0..] ps)
  where l = length ps

tr x = trace (show x) x

squash :: Int -> (Int, Pattern a) -> Pattern a
squash n (i, p@(Sequence _)) = Sequence $ \r -> concatMap doBit (bits r)
  where o' = (fromIntegral i)%(fromIntegral n)
        d' = 1%(fromIntegral n)
        subR o = ((cyc o) + o', d')
        doBit (o,d) = mapFsts scaleOut $ maybe [] ((arc p) . scaleIn) (subRange (o,d) (subR o))
        -- scaleIn (o,d) = (o-o',d* (fromIntegral n))
        scaleIn (o, Just d) = ((cyc o)+((o-(cyc o)-o')*(fromIntegral n)), Just (d*(fromIntegral n)))
        scaleIn (o, Nothing) = ((cyc o)+((o-(cyc o)-o')*(fromIntegral n)), Nothing)
        scaleOut (o,d) = ((cyc o)+o'+((o-(cyc o))/(fromIntegral n)), d/(fromIntegral n))

squash n (i, p@(Signal _)) = Signal $ f
  where f t | (t - cyc t) >= t' && (t - cyc t) < (t'+d') = (at p) $ scaleIn t
            | otherwise = []
        t' = (fromIntegral i)%(fromIntegral n)
        d' = 1%(fromIntegral n)
        scaleIn t  = (cyc t)+((t-(cyc t)-t')*(fromIntegral n))

cyc = fromIntegral . floor

subRange :: (Rational, Maybe Rational) -> Range -> Maybe (Rational, Maybe Rational)
subRange (o, Just d) (o',d') | d'' > 0 = Just (o'', Just d'')
                             | otherwise = Nothing
  where o'' = max o (o')
        d'' = (min (o+d) (o'+d')) - o''
subRange (o, Nothing) (o',d') | o >= o' && o < (o' + d') = Just (o, Nothing)
                              | otherwise = Nothing

-- chop range into ranges of unit cycles
bits :: (Rational, Maybe Rational) -> [(Rational, Maybe Rational)]
bits r@(_, Nothing) = [r]
bits (_, Just 0) = []
bits (o, Just d) = (o, Just d'):bits (o+d',Just (d-d'))
  where d' = min ((fromIntegral $ (floor o) + 1) - o) d

combine :: [Pattern a] -> Pattern a
combine ps = foldr f silence ps
  where f (Sequence a) (Sequence b) = Sequence $ \r -> (a r) ++ (b r)
        f (Signal a) (Sequence b) = Signal $ \t -> (a t) ++ (map snd $ b (t, Nothing))
        f a b = f b a

foo (Sequence a)  = a
foo _ = error "oops"

patToOnsets :: (Rational, Maybe Rational) -> Pattern a -> [Event a]
patToOnsets _ (Signal _) = [] --map (\x -> (t, x)) (a t)
patToOnsets r (Sequence a) = a r

filterEvents :: (Event a -> Bool) -> Pattern a -> Pattern a
filterEvents f (Sequence a) = Sequence $ \r -> filter f $ a r

-- Filter out events that start before range
filterOffsets :: Pattern a -> Pattern a
filterOffsets (Sequence a) = Sequence $ \r -> filter ((>= (fst r)). fst . fst) $ a r

patToRelOnsets :: (Rational, Maybe Rational) -> Pattern a -> [(Double, a)]
patToRelOnsets _ (Signal _) = []
patToRelOnsets (s, Just d) p = mapFsts (fromRational . (/ d) . (subtract s) . fst) $ patToOnsets (s, Just d) (filterOffsets p)
patToRelOnsets (s, Nothing) _ = []

mapEvents :: (Event a -> Event a) -> Pattern a -> Pattern a
mapEvents f (Sequence a) = Sequence $ \r -> map f (a r)
mapEvents f (Signal a) = Signal $ \t -> map (\x -> snd $ f ((t,0), x)) (a t)

-- Maps time of events from an unmapped time range..  
-- Generally not what you want..

mapEventRange :: (Rational -> Rational) -> Pattern a -> Pattern a
mapEventRange f p = mapEvents (mapFst f') p
  where f' (s, d) = (f s, (f (s + d)) - (f s))

mapOnset :: (Rational -> Rational) -> Pattern a -> Pattern a
mapOnset f (Signal a) = Signal $ \t -> a (f t)
mapOnset f (Sequence a) = Sequence $ \(s, d) -> a (f s, d)

-- Function applied to both onset (start) and offset (start plus duration)
mapRange :: (Rational -> Rational) -> Pattern a -> Pattern a
mapRange f p@(Sequence a) = Sequence a'
  where a' (s, Just d) = a (f s, Just $ (f (s + d)) - (f s))
        a' (s, Nothing) = a (f s, Nothing)
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

sinewave :: Pattern Double
sinewave = Signal $ \t -> [(sin . (pi * 2 *)) (fromRational t)]

sinewave1 :: Pattern Double
sinewave1 = fmap ((/ 2) . (+ 1))  sinewave

