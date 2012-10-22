{-# LANGUAGE NoMonomorphismRestriction #-}

module Acid where

import Stream
import Pattern
import Parse
import Sound.OpenSoundControl
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Control.Concurrent
import Network.Netclock.Client

dit :: OscShape
dit = OscShape {path = "/point",
                params = [ S "port" Nothing,
                           F "angle" (Just 0),
                           F "radius" (Just 0)
                         ],
                timestamp = False
               }


ditstream name = stream "127.0.0.1" "127.0.0.1" name "127.0.0.1" 6014 dit

port        = makeS dit "port"
angle       = makeF dit "angle"
radius      = makeF dit "radius"

sendSam = do forkIO $ clocked "sam" "127.0.0.1" "127.0.0.1" 1 samTick
             return ()

samTick x ticks = do dit <- openUDP "127.0.0.1" 6014
                     send dit $ Message "/sam" []
                     close dit
                     putStrLn "Sam."