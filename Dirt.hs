{-# LANGUAGE NoMonomorphismRestriction #-}
  
module Dirt where

import Stream
import Pattern
import Parse
import Sound.OpenSoundControl
import qualified Data.Map as Map
import Control.Applicative
import Control.Concurrent.MVar
import Visual
import Data.Colour.SRGB
import Data.Colour.Names
import Data.Hashable
import Data.Bits
import Data.Maybe

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
                            F "accellerate" (Just 0),
                            F "shape" (Just 0)
                          ],
                 timestamp = True
                }


--steps = 16
--channels = 4
--x = Map.insert (params dirt !! 0) (Just $ String "chin/0") $ defaultMap dirt
--x' pan = Map.insert (params dirt !! 4) (Just $ Float pan) $ x
--c = Cycle $ map (\i -> (Arc (atom $ x' (channels * (fromIntegral i / fromIntegral steps))) (fromIntegral i / fromIntegral steps) 0)) [0 .. (steps - 1)]

dirtstream name = stream "127.0.0.1" "127.0.0.1" name "127.0.0.1" 7771 dirt

dirtToColour :: OscSequence -> Sequence ColourD
dirtToColour p = s
  where s = fmap (\x -> maybe black (maybe black datumToColour) (Map.lookup (param dirt "sound") x)) p

datumToColour :: Datum -> ColourD
datumToColour = stringToColour . show

stringToColour :: String -> ColourD
stringToColour s = sRGB (r/256) (g/256) (b/256)
  where i = (hash s) `mod` 16777216
        r = fromIntegral $ (i .&. 0xFF0000) `shiftR` 16;
        g = fromIntegral $ (i .&. 0x00FF00) `shiftR` 8;
        b = fromIntegral $ (i .&. 0x0000FF);


visualcallback :: IO (OscSequence -> IO ())
visualcallback = do t <- ticker
                    mv <- startVis t
                    let f p = do let p' = dirtToColour p
                                 swapMVar mv p'
                                 return ()
                    return f

dirtyvisualstream name = do cb <- visualcallback
                            streamcallback cb "127.0.0.1" "127.0.0.1" name "127.0.0.1" 7771 dirt
                            

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
shape        = makeF dirt "shape"

sample :: String -> Int -> String
sample name n = name ++ "/" ++ (show n)

striate :: Int -> OscSequence -> OscSequence
striate n p = cat $ map (\x -> off (fromIntegral x) p) [0 .. n-1]
  where off i p = p ~~ begin (atom (fromIntegral i / fromIntegral n) :: Sequence Double) ~~ end (atom (fromIntegral (i+1) / fromIntegral n) :: Sequence Double)
        
striateO :: OscSequence -> Int -> Double -> OscSequence
striateO p n o = cat $ map (\x -> off (fromIntegral x) p) [0 .. n-1]
  where off i p = p ~~ begin ((atom $ (fromIntegral i / fromIntegral n) + o) :: Sequence Double) ~~ end ((atom $ (fromIntegral (i+1) / fromIntegral n) + o) :: Sequence Double)

metronome = slow 2 $ sound (p "[odx, [hh]*8]")

