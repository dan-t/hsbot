{-# LANGUAGE PatternGuards #-}

import System.Exit (exitSuccess)
import qualified Debug.Trace as T
import Control.Applicative ((<$>))
import qualified Data.Vector as Vec
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Gfx as GFX
import qualified Gamgine.Math.Vect as V
import qualified Gamgine.Math.Matrix as M
import qualified Gamgine.Math.Utils as MU
import Gamgine.Math.Vect
import Gamgine.Gfx ((<<<))
import qualified Grid as G

gridWidth   = 10 :: Int
gridHeight  = 10 :: Int

dGridWidth  = fromIntegral gridWidth
dGridHeight = fromIntegral gridHeight

borderWidth = (max dGridWidth dGridHeight) / 8
edgeLength  =  1 :: Double


main :: IO ()
main = do
   initGLFW
   appLoop $ G.mkGrid gridWidth gridHeight


appLoop :: G.Grid -> IO ()
appLoop grid = do
   GL.glClearColor 0 0 0 0
   GL.glClear (fromIntegral GL.gl_COLOR_BUFFER_BIT)
   G.renderGrid grid
   GLFW.swapBuffers
   appLoop grid


initGLFW :: IO ()
initGLFW = do
   GLFW.initialize
   GLFW.openWindow GLFW.defaultDisplayOptions {
      GLFW.displayOptions_width             = 800,
      GLFW.displayOptions_height            = 800,
      GLFW.displayOptions_windowIsResizable = True
      }

   GLFW.setWindowBufferSwapInterval 1
   GLFW.setWindowSizeCallback resize
   GLFW.setWindowCloseCallback quit
   GLFW.setMousePositionCallback $ \x y -> return ()

   where
      resize width height = do
         let M.Frustum {M.right = right, M.top = top} = frustum (width, height)

	 GL.glViewport 0 0 (fromIntegral width) (fromIntegral height)
	 GL.glMatrixMode GL.gl_PROJECTION
	 GL.glLoadIdentity
         GL.glOrtho 0 (GFX.floatToFloat right) 0 (GFX.floatToFloat top) (-1) 1
         
         GL.glMatrixMode GL.gl_MODELVIEW
         GL.glLoadIdentity
         gridTrans <- gridTranslation
         GL.glTranslatef <<< gridTrans
         
      quit = GLFW.closeWindow >> GLFW.terminate >> exitSuccess


gridTranslation :: IO V.Vect
gridTranslation = do
   M.Frustum {M.right = r, M.top = t} <- frustum <$> GLFW.getWindowDimensions
   let transX = (r - dGridWidth) / 2
       transY = (t - dGridHeight) / 2
   return (transX:.transY:.0)


frustum :: (Int, Int) -> M.Frustum
frustum (width, height) =
   let dWidth  = fromIntegral width  :: Double
       dHeight = fromIntegral height :: Double
       maxSide = max dGridWidth dGridHeight + (2 * borderWidth)
       (right, top) | dWidth < dHeight = (maxSide, maxSide * (dHeight / dWidth))
                    | otherwise        = (maxSide * (dWidth / dHeight), maxSide)
       in M.Frustum 0 right 0 top (-1) 1
