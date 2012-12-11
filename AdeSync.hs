import Sound.OpenSoundControl
import Network.Netclock.Client
import Control.Concurrent (threadDelay, forkIO)
  
tpb = 4
address = "127.0.0.1"

adeip =  "10.0.0.3"   
adeport = 1777

main = do putStrLn "start"
          clocked "adesync" address address tpb ot

ot :: BpsChange -> Int -> IO ()
ot change tick = do putStrLn "tick"
                    ade <- openUDP adeip adeport
                    let m = Message "/PureEvents/Beat" [Int tick]
                    threadDelay $ 1000000 `div` 18
                    send ade m
                    close ade
                    return ()

