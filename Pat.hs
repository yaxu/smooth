module Pat where

import Control.Applicative
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Ratio
import Debug.Trace

type T = (Rational, Rational)
type Event a = (T, a)
  
data Seq a = Seq {events :: [Event a]}

data Pat a = Pat {arc :: T -> Seq a}
           | Sig {at :: Rational -> Seq a}

instance Functor Seq where
  fmap f = Seq . mapSnds f . events

instance Functor Pat where
  fmap f (Pat a) = Pat $ fmap f . a
  fmap f (Sig a) = Sig $ fmap f . a

instance Applicative Seq where
  pure x = Seq [((0,1), x)]
  (Seq fs) <*> (Seq xs) = Seq xs'
    where xs' = concatMap (\f -> map (\(t, x) -> (t, (snd f) x)) $ filter (startsIn f) xs) fs

startsIn :: Event a -> Event b -> Bool
startsIn (t, _) (t', _) = (fst t') >= (fst t) && (fst t' <= (fst t + snd t))

instance Applicative Pat where
  pure x = rep x
  (Pat fs) <*> (Pat xs) = Pat $ \t -> fs t <*> xs t
  (Sig fs) <*> (Sig xs) = Sig $ \i -> fs i <*> xs i
  (Pat fs) <*> (Sig xs) = Pat $ s
    where s t = Seq $ concatMap (\(t', f) -> mapSnds f (events $ xs $ fst t')) (events $ fs t)
  (Sig fs) <*> (Pat xs) = Sig $ s
    where s i = Seq $ concatMap (\(t', f) -> mapSnds f (events $ xs $ t') ) (events $ fs i)

rep :: a -> Pat a
rep x = Pat $ \(s, d) -> Seq (map (\n -> ((fromIntegral n, 1), x)) [(ceiling s) .. (ceiling $ s+d) - 1])

-- instance Applicative Pat where
--   pure x = rep x
--   fs <*> xs = Pat $ \t -> (arc fs) t <*> (arc xs) t



-- mapFst :: (a -> b) -> (a, c) -> (b, c)
-- mapFst f (x,y) = (f x,y)

-- mapFsts :: (a -> b) -> [(a, c)] -> [(b, c)]
-- mapFsts = map . mapFst

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

mapSnds :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnds = fmap . mapSnd
