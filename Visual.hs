module Visual where

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT
import Parse
import Pattern
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB

drawOneLine :: Vertex2 GLfloat -> Vertex2 GLfloat -> IO ()
drawOneLine p1 p2 = renderPrimitive Lines $ do vertex p1; vertex p2

drawRect :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
drawRect x y x' y' 
  = renderPrimitive Polygon $ do vertex$Vertex2 x y
                                 vertex$Vertex2 x y'
                                 vertex$Vertex2 x' y'
                                 vertex$Vertex2 x' y
                                 return ()

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat

w = 1024
lw = 150

translatef = translate :: Vector3 GLfloat -> IO ()


drawEvent e = do lineWidth $= lw'
                 preservingMatrix $ drawEvent' lw' e
  where lw' = lw / (fromIntegral $ length $ snd e)

drawEvent' :: GLfloat -> (Event [ColourD]) -> DisplayCallback
drawEvent' _ (_, []) = return ()
drawEvent' lw' ((t, d), c:cs) 
  = do let (Data.Colour.SRGB.RGB r g b) = toSRGB c
       color (Color3 r g b)
       --drawOneLine (Vertex2 (w * (fromRational t)) y) (Vertex2 (w * (fromRational $ t+d)) y)
       drawRect (w * (fromRational t)) 0 (w * (fromRational $ t+d)) lw'
       translatef (Vector3 0 (lw') 0)
       drawEvent' lw' ((t,d), cs)
  where y = (lw'/2)

display :: MVar Rational -> MVar (Sequence ColourD) -> DisplayCallback
display t mv = do
   clear [ ColorBuffer ]
   ticks <- readMVar t
   p <-readMVar mv
   mapM_ drawEvent (map (mapFst (\(t,d) -> ((t - (ticks/2))/speed,d/speed))) $ range (segment p) ((ticks/2), Just speed))
   flush
   where speed = 4

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   ortho2D 0 (fromIntegral w) 0 (fromIntegral h)
   -- the following line is not in the original example, but it's good style...
   matrixMode $= Modelview 0

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _ _ _ _ = return ()

animate = do
  postRedisplay Nothing
  addTimerCallback 16 $ animate

--  Request double buffer display mode.
--  Register mouse input callback functions
startVis :: MVar Rational -> IO (MVar (Sequence ColourD))
startVis t = 
  do initialize "smooth" []
     initialDisplayMode $= [ SingleBuffered, RGBMode ]
     initialWindowSize $= Size 1024 150
     initialWindowPosition $= Position 100 100
     createWindow "smooth"
     myInit
     mp <- newMVar (pure black)
     displayCallback $= (display t mp)
     addTimerCallback 80 $ animate
     reshapeCallback $= Just reshape
     keyboardMouseCallback $= Just keyboard
     forkIO $ mainLoop
     return mp
