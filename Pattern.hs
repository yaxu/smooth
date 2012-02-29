import Data.Fixed

type Range = (Double, Double)
type Event a = (Double, a)
data Pattern a = Pattern {events :: [Event a], period :: Double}

instance (Show a) => Show (Pattern a) where
  show (Pattern _ 0) = ""
  show p = show $ events p

class Patternable p where
  pattern :: p a -> Pattern a

instance Patternable [] where
  pattern xs = Pattern r 1
    where r = map (\x -> ((fromIntegral x) / (fromIntegral $ length xs), 
                          xs !! x)) [0 .. (length xs) - 1]

rev :: Pattern a -> Pattern a
rev = mapTime (1 -)

(<~) :: Pattern a -> Double -> Pattern a
p <~ d = mapTime (\x -> mod' (x - d) 1) p

(~>) :: Pattern a -> Double -> Pattern a
p ~> d = p <~ (0-d)


every :: Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
every 0 _ p = p
every n f p = cat $ (take (n-1) $ repeat p) ++ [f p]

cat :: [Pattern a] -> Pattern a
cat ps = Pattern (concatMap events ps') n
  where ps' = map (\(p, d) -> mapTime (+ (fromIntegral d)) p) $ zip ps [0 ..]
        n = (sum $ (map period) ps)

mapTime :: (Double -> Double) -> Pattern a -> Pattern a
mapTime f p = p {events = map (mapFst f) (events p)}

sinewave :: Int -> [Double]
sinewave n =  map (\x -> sin (step * fromIntegral x)) [0 .. n-1]
  where step = (pi * 2.0) / fromIntegral n

sinewave1 :: Int -> [Double]
sinewave1 = map ((/ 2) . (+ 1)) . sinewave

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)
