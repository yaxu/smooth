{-# LANGUAGE DeriveDataTypeable #-}

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

data Sequence a = Sequence {range :: Range -> [Event a]}
                deriving (Typeable)
data Signal a = Signal {at :: Time -> [a]}
              deriving (Typeable)

instance (Show a) => Show (Sequence a) where
  show p@(Sequence _) = show $ range p (0, Just 1)

instance (Show a) => Show (Signal a) where
  show p@(Signal _) = "~signal~"

class (Functor p, Applicative p) => Pattern p where
  pt :: (p a) -> Time -> [a]
  atom :: a -> p a
  silence :: p a
  toSignal :: p a -> Signal a
  toSignal p = Signal $ \t -> pt p t
  squash :: Int -> (Int, p a) -> p a
  combine' :: p a -> p a -> p a
  mapOnset :: (Time -> Time) -> p a -> p a
  mapTime :: (Time -> Time) -> p a -> p a
  mapTime = mapOnset
  mapTimeOut :: (Time -> Time) -> p a -> p a

combine :: (Pattern p) =>  [p a] -> p a
combine ps = foldr combine' silence ps

instance Pattern Signal where
  pt = at
  atom x = Signal $ const [x]
  silence = Signal $ const []
  toSignal = id
  squash = squashSignal
  combine' (Signal a) (Signal b) = Signal $ \t -> (a t) ++ (b t)
  mapOnset f (Signal a) = Signal $ \t -> a (f t)
  mapTimeOut _ = id

instance Pattern Sequence where
  pt p t = map snd $ range p (t, Nothing)
  atom x = Sequence f
    where f (s, Nothing) = [((sam s, 1), x)]
          f (s, Just d) = map 
                          (\t -> ((fromIntegral t, 1), x)) 
                          [floor s .. (ceiling (s + d)) - 1]
  silence = Sequence $ const []
  squash = squashSequence
  combine' (Sequence a) (Sequence b) = Sequence $ \r -> (a r) ++ (b r)
  mapOnset f (Sequence a) = Sequence $ \(s, d) -> a (f s, d)
  -- Function applied to both onset (start) and offset (start plus duration)
  mapTime f (Sequence a) = Sequence a'
    where a' (s, Just d) = a (f s, Just $ (f (s + d)) - (f s))
          a' (s, Nothing) = a (f s, Nothing)
  mapTimeOut f p = mapEvents (mapFst f') p
    where f' (s, d) = (f s, (f (s + d)) - (f s))

squashSignal :: Int -> (Int, Signal a) -> Signal a
squashSignal n (i, p@(Signal _)) = Signal $ f
  where f t | (t - sam t) >= t' && (t - sam t) < (t'+d') = (at p) $ scaleIn t
            | otherwise = []
        t' = (fromIntegral i)%(fromIntegral n)
        d' = 1%(fromIntegral n)
        scaleIn t  = (sam t)+((t-(sam t)-t')*(fromIntegral n))

squashSequence :: Int -> (Int, Sequence a) -> Sequence a
squashSequence n (i, p@(Sequence _)) 
  = Sequence $ \r -> concatMap doBit (bits r)
  where o' = (fromIntegral i)%(fromIntegral n)
        d' = 1%(fromIntegral n)
        subR o = ((sam o) + o', d')
        doBit (o,d) = mapFsts scaleOut $ maybe [] ((range p) . scaleIn) (subRange (o,d) (subR o))
        scaleIn (o, Just d) = ((sam o)+((o-(sam o)-o')*(fromIntegral n)), Just (d*(fromIntegral n)))
        scaleIn (o, Nothing) = ((sam o)+((o-(sam o)-o')*(fromIntegral n)), Nothing)
        scaleOut (o,d) = ((sam o)+o'+((o-(sam o))/(fromIntegral n)), d/(fromIntegral n))


instance Functor Sequence where
  fmap f (Sequence a) = Sequence $  fmap (fmap (mapSnd f)) a

instance Functor Signal where
  fmap f (Signal a) = Signal $ fmap (fmap f) a

instance Applicative Sequence where
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

instance Applicative Signal where
  pure = atom
  (Signal fs) <*> (Signal xs) = Signal $ \t -> (fs t) <*> (xs t)


cat :: (Pattern p) => [p b] -> p b
cat ps = combine $ map (squash l) (zip [0..] ps)
  where l = length ps

listToPat :: Pattern p => [a] -> p a
listToPat = cat . map atom

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

(Sequence fs) <~> (Signal xs) = 
  Sequence $ \r -> concatMap (\((o,d), f) -> 
                               map (\x -> ((o,d), f x)) (xs o)) (fs r)

infixl 4 <~>

{-
  (Signal fs) <*> px@(Sequence _) = 
    Signal $ \t -> concatMap (\(_, x) -> map (\f -> f x) (fs t)) (range px (t,Nothing))
-}

--listToSeq :: [a] -> Pattern a
--listToSeq = cat . map atom


sam :: Time -> Time
sam = fromIntegral . floor

filterEvents :: (Event a -> Bool) -> Sequence a -> Sequence a
filterEvents f (Sequence a) = Sequence $ \r -> filter f $ a r

-- Filter out events that start before range
filterOffsets :: Sequence a -> Sequence a
filterOffsets (Sequence a) = Sequence $ \r -> filter ((>= (fst r)). fst . fst) $ a r

seqToRelOnsets :: Range -> Sequence a -> [(Double, a)]
seqToRelOnsets (s, Just d) p = mapFsts (fromRational . (/ d) . (subtract s) . fst) $ range (filterOffsets p) (s, Just d)
seqToRelOnsets (s, Nothing) _ = []

mapEvents :: (Event a -> Event a) -> Sequence a -> Sequence a
mapEvents f (Sequence a) = Sequence $ \r -> map f (a r)

(<~) :: Pattern p => Time -> p a -> p a
(<~) t p = mapTimeOut (+ t) $ mapTime (subtract t) p

(~>) :: Pattern p => Time -> p a -> p a
(~>) t p = mapTimeOut (subtract t) $ mapTime (+ t) p

-- Maps time of events from an unmapped time range..  
-- Generally not what you want..

--mapEventOnset :: (Time -> Time) -> Sequence a -> Sequence a
--mapEventOnset f p = mapEvents (mapFst f') p
--  where f' (s, d) = (f s, d)

{-
mapEvents f (Signal a) = Signal $ \t -> map (\x -> snd $ f ((t,0), x)) (a t)

-}

slow :: Pattern p => Time -> p a -> p a
slow 1 p = p
slow r p = mapTimeOut (* r) $ mapTime (/ r) p

density :: Pattern p => Time -> p a -> p a
density 1 p = p
density r p = mapTimeOut (/ r) $ mapTime (* r) p

every :: Pattern p => Int -> (p a -> p a) -> p a -> p a
every 0 _ p = p
every n f p = slow (fromIntegral n %1) $ cat $ (take (n-1) $ repeat p) ++ [f p]

sinewave :: Signal Double
sinewave = Signal $ \t -> [(sin . (pi * 2 *)) (fromRational t)]

sinewave1 :: Signal Double
sinewave1 = fmap ((/ 2) . (+ 1))  sinewave

triwave1 :: Signal Double
triwave1 = Signal $ \x -> [mod' (fromRational x) 1]

triwave :: Signal Double
triwave = ((subtract 1) . (* 2)) <$> triwave1

squarewave1 :: Signal Double
squarewave1 = Signal f 
  where f x = [fromIntegral $ floor $ (mod' (fromRational x) 1) * 2]

squarewave :: Signal Double
squarewave = ((subtract 1) . (* 2)) <$> squarewave1

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)

mapFsts :: (a -> b) -> [(a, c)] -> [(b, c)]
mapFsts = map . mapFst

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

mapSnds :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnds = fmap . mapSnd

