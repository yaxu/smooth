{-# LANGUAGE NoMonomorphismRestriction #-}
  
module Dirt where

import Stream
import Pattern
import Parse
import Sound.OpenSoundControl
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Visual

dirt :: OscShape
dirt = OscShape {path = "/play",
                 params = [ S "sound" Nothing,
                            F "offset" (Just 0),
                            F "begin" (Just 0),
                            F "end" (Just 1),
                            F "speed" (Just 1),
                            F "pan" (Just 0.5),
                            F "velocity" (Just 0),
                            S "vowel" (Just ""),
                            F "cutoff" (Just 0),
                            F "resonance" (Just 0),
                            F "accellerate" (Just 0)
                          ],
                 timestamp = True
                }


--steps = 16
--channels = 4
--x = Map.insert (params dirt !! 0) (Just $ String "chin/0") $ defaultMap dirt
--x' pan = Map.insert (params dirt !! 4) (Just $ Float pan) $ x
--c = Cycle $ map (\i -> (Arc (atom $ x' (channels * (fromIntegral i / fromIntegral steps))) (fromIntegral i / fromIntegral steps) 0)) [0 .. (steps - 1)]

dirtstream name = stream "127.0.0.1" "127.0.0.1" name "127.0.0.1" 7771 dirt

sound        = makeS dirt "sound"
offset       = makeF dirt "offset"
begin        = makeF dirt "begin"
end          = makeF dirt "end"
speed        = makeF dirt "speed"
pan          = makeF dirt "pan"
velocity     = makeF dirt "velocity"
vowel        = makeS dirt "vowel"
cutoff       = makeF dirt "cutoff"
resonance    = makeF dirt "resonance"
accellerate  = makeF dirt "accellerate"

sample :: String -> Int -> String
sample name n = name ++ "/" ++ (show n)

striate :: OscSequence -> Int -> OscSequence
striate p n = cat $ map (\x -> off (fromIntegral x) p) [0 .. n-1]
  where off i p = p ~~ begin (atom (fromIntegral i / fromIntegral n) :: Sequence Double) ~~ end (atom (fromIntegral (i+1) / fromIntegral n) :: Sequence Double)
        
striateO :: OscSequence -> Int -> Double -> OscSequence
striateO p n o = cat $ map (\x -> off (fromIntegral x) p) [0 .. n-1]
  where off i p = p ~~ begin ((atom $ (fromIntegral i / fromIntegral n) + o) :: Sequence Double) ~~ end ((atom $ (fromIntegral (i+1) / fromIntegral n) + o) :: Sequence Double)

