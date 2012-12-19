{-
   Lines.hs (adapted from lines.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates geometric primitives and their attributes.
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT
import Parse
import Pattern
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB

drawOneLine :: Vertex2 GLfloat -> Vertex2 GLfloat -> IO ()
drawOneLine p1 p2 = renderPrimitive Lines $ do vertex p1; vertex p2

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat

w = 1024
lw = 400

translatef = translate :: Vector3 GLfloat -> IO ()


drawEvent e = do lineWidth $= lw'
                 preservingMatrix $ drawEvent' lw' e
  where lw' = lw / (fromIntegral $ length $ snd e)

drawEvent' :: GLfloat -> (Event [ColourD]) -> DisplayCallback
drawEvent' _ (_, []) = return ()
drawEvent' lw' ((t, d), c:cs) 
  = do let (Data.Colour.SRGB.RGB r g b) = toSRGB c
       color (Color3 r g b)
       drawOneLine (Vertex2 (w * (fromRational t)) y) (Vertex2 (w * (fromRational $ t+d)) y)
       translatef (Vector3 0 (lw') 0)
       drawEvent' lw' ((t,d), cs)
  where y = (lw'/2)

display :: (MVar (Sequence ColourD)) -> DisplayCallback
display mv = do
   clear [ ColorBuffer ]
   p <-readMVar mv
   mapM_ drawEvent (range (segment p) (0, Just 1))
   flush

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
start :: IO (MVar (Sequence ColourD))
start = do initialize "smooth" []
           initialDisplayMode $= [ SingleBuffered, RGBMode ]
           initialWindowSize $= Size 1024 400
           initialWindowPosition $= Position 100 100
           createWindow "smooth"
           myInit
           mp <- newMVar (p "[white [red, [green orange] blue] red, tomato yellow]")
           displayCallback $= (display mp)
           addTimerCallback 100 $ animate  -- refresh every 1/10sec
           reshapeCallback $= Just reshape
           keyboardMouseCallback $= Just keyboard
           forkIO $ mainLoop
           return mp
