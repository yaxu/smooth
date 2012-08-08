import Sound.OpenSoundControl
import Network.Netclock.Client
  
tpb = 4
address = "127.0.0.1"

adeip =  "192.168.1.3"   
adeport = 1777

main = clocked "adesync" address address tpb ot

ot :: BpsChange -> Int -> IO ()
ot change tick = do putStrLn "tick"
                    ade <- openUDP adeip adeport
                    let m = Message "/PureEvents/Beat" [Int tick]
                    send ade m
                    close ade
                    return ()

