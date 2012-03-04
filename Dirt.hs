module Dirt where

import Stream
import Pattern
import Sound.OpenSoundControl
import qualified Data.Map as Map
import Control.Concurrent.MVar

dirt :: OscShape
dirt = OscShape {path = "/play",
                 params = [ S "sample" Nothing,
                            F "offset" (Just 0),
                            F "duration" (Just 1),
                            F "speed" (Just 1),
                            F "pan" (Just 0.5),
                            F "velocity" (Just 0)
                          ],
                 timestamp = True
                }


steps = 16
channels = 4
x = Map.insert (params dirt !! 0) (Just $ String "chin/0") $ defaultMap dirt
x' pan = Map.insert (params dirt !! 4) (Just $ Float pan) $ x
c = Cycle $ map (\i -> (Arc (Atom $ x' (channels * (fromIntegral i / fromIntegral steps))) (fromIntegral i / fromIntegral steps) Nothing)) [0 .. (steps - 1)]

startdirt = start "127.0.0.1" "127.0.0.1" "deardirt" "127.0.0.1" 7771 dirt