module Pattern where

import Control.Applicative
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Ratio
import Debug.Trace
import Data.Typeable

type Time = Rational
type Arc = (Time, Time)
type Range = (Time, Maybe Time)

type Event a = (Arc, a)

data Pattern a = Sequence {range :: Range -> [Event a]}
               | Signal {at :: Time -> [a]}

instance (Show a) => Show (Pattern a) where
  show p@(Sequence _) = show $ range p (0, Just 1)
  show p@(Signal _) = "~signal~"

silence = Sequence $ const []
silenceSig = Signal $ const []

sam :: Time -> Time
sam = fromIntegral . floor

atom :: a -> Pattern a
atom x = Sequence f
  where f (s, Nothing) = [((sam s, 1), x)]
        f (s, Just d) = map 
                        (\t -> ((fromIntegral t, 1), x)) 
                        [floor s .. (ceiling (s + d)) - 1]

instance Functor Pattern where
  fmap f (Sequence a) = Sequence $  fmap (fmap (mapSnd f)) a
  fmap f (Signal a) = Signal $ fmap (fmap f) a

instance Applicative Pattern where
  pure = atom
  (Sequence fs) <*> (Sequence xs) = 
    Sequence $ \r ->
      concatMap
      (\((o,d),x) -> map 
                     (\(r', f) -> (r', f x))
                     (filter
                      (\((o',d'),_) -> (o' >= o) && (o' < (o+d)))
                      (fs r)
                     )
      )
      (xs r)
  (Signal fs) <*> (Signal xs) = Signal $ \t -> (fs t) <*> (xs t)
  (Signal fs) <*> px@(Sequence _) = 
    Signal $ \t -> concatMap (\(_, x) -> map (\f -> f x) (fs t)) (range px (t,Nothing))
  (Sequence fs) <*> (Signal xs) = 
    Sequence $ \r -> concatMap (\((o,d), f) -> 
                                map (\x -> ((o,d), f x)) (xs o)) (fs r)

cat :: [Pattern a] -> Pattern a
cat ps = combine $ map (squash l) (zip [0..] ps)
  where l = length ps

listToPat :: [a] -> Pattern a
listToPat = cat . map atom

tr x = trace (show x) x

squash :: Int -> (Int, Pattern a) -> Pattern a
squash n (i, p@(Sequence _)) = Sequence $ \r -> concatMap doBit (bits r)
  where o' = (fromIntegral i)%(fromIntegral n)
        d' = 1%(fromIntegral n)
        subR o = ((sam o) + o', d')
        doBit (o,d) = mapFsts scaleOut $ maybe [] ((range p) . scaleIn) (subRange (o,d) (subR o))
        scaleIn (o, Just d) = ((sam o)+((o-(sam o)-o')*(fromIntegral n)), Just (d*(fromIntegral n)))
        scaleIn (o, Nothing) = ((sam o)+((o-(sam o)-o')*(fromIntegral n)), Nothing)
        scaleOut (o,d) = ((sam o)+o'+((o-(sam o))/(fromIntegral n)), d/(fromIntegral n))

squash n (i, p@(Signal _)) = Signal $ f
  where f t | (t - sam t) >= t' && (t - sam t) < (t'+d') = (at p) $ scaleIn t
            | otherwise = []
        t' = (fromIntegral i)%(fromIntegral n)
        d' = 1%(fromIntegral n)
        scaleIn t  = (sam t)+((t-(sam t)-t')*(fromIntegral n))

subRange :: Range -> Arc -> Maybe Range
subRange (o, Just d) (o',d') | d'' > 0 = Just (o'', Just d'')
                             | otherwise = Nothing
  where o'' = max o (o')
        d'' = (min (o+d) (o'+d')) - o''
subRange (o, Nothing) (o',d') | o >= o' && o < (o' + d') = Just (o, Nothing)
                              | otherwise = Nothing

-- chop range into ranges of unit cycles
bits :: Range -> [Range]
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

patToOnsets :: Range -> Pattern a -> [Event a]
patToOnsets _ (Signal _) = [] --map (\x -> (t, x)) (a t)
patToOnsets r (Sequence a) = a r

filterEvents :: (Event a -> Bool) -> Pattern a -> Pattern a
filterEvents f (Sequence a) = Sequence $ \r -> filter f $ a r

-- Filter out events that start before range
filterOffsets :: Pattern a -> Pattern a
filterOffsets (Sequence a) = Sequence $ \r -> filter ((>= (fst r)). fst . fst) $ a r

patToRelOnsets :: Range -> Pattern a -> [(Double, a)]
patToRelOnsets _ (Signal _) = []
patToRelOnsets (s, Just d) p = mapFsts (fromRational . (/ d) . (subtract s) . fst) $ patToOnsets (s, Just d) (filterOffsets p)
patToRelOnsets (s, Nothing) _ = []

mapEvents :: (Event a -> Event a) -> Pattern a -> Pattern a
mapEvents f (Sequence a) = Sequence $ \r -> map f (a r)
mapEvents f (Signal a) = Signal $ \t -> map (\x -> snd $ f ((t,0), x)) (a t)

-- Maps time of events from an unmapped time range..  
-- Generally not what you want..

mapEventArc :: (Time -> Time) -> Pattern a -> Pattern a
mapEventArc f p = mapEvents (mapFst f') p
  where f' (s, d) = (f s, (f (s + d)) - (f s))

mapEventOnset :: (Time -> Time) -> Pattern a -> Pattern a
mapEventOnset f p = mapEvents (mapFst f') p
  where f' (s, d) = (f s, d)

mapOnset :: (Time -> Time) -> Pattern a -> Pattern a
mapOnset f (Signal a) = Signal $ \t -> a (f t)
mapOnset f (Sequence a) = Sequence $ \(s, d) -> a (f s, d)

-- Function applied to both onset (start) and offset (start plus duration)
mapTime :: (Time -> Time) -> Pattern a -> Pattern a
mapTime f p@(Sequence a) = Sequence a'
  where a' (s, Just d) = a (f s, Just $ (f (s + d)) - (f s))
        a' (s, Nothing) = a (f s, Nothing)
mapTime f p@(Signal a) = mapOnset f p

(<~) :: Time -> Pattern a -> Pattern a
(<~) t p = mapEventArc (+ t) $ mapTime (subtract t) p

(~>) :: Time -> Pattern a -> Pattern a
(~>) t p = mapEventArc (subtract t) $ mapTime (+ t) p

slow :: Time -> Pattern a -> Pattern a
slow 1 p = p
slow r p = mapEventArc (* r) $ mapTime (/ r) p

density :: Time -> Pattern a -> Pattern a
density 1 p = p
density r p = mapEventArc (/ r) $ mapTime (* r) p

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

triwave1 :: Pattern Double
triwave1 = Signal $ \x -> [mod' (fromRational x) 1]

triwave :: Pattern Double
triwave = ((subtract 1) . (* 2)) <$> triwave1

squarewave1 :: Pattern Double
squarewave1 = Signal f 
  where f x = [fromIntegral $ floor $ (mod' (fromRational x) 1) * 2]

squarewave :: Pattern Double
squarewave = ((subtract 1) . (* 2)) <$> squarewave1

{-rev :: Pattern a -> Pattern a
rev = (mapEvents (mapFst swapSD)) . (mapEventArc revTime) . (mapOnset revTime)
  where revTime x = sam x + (1 - (x - sam x))
        swapSD (s,d) = ((s+d, 0-d))
-}