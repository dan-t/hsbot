{-# LANGUAGE PatternGuards #-}

import System.Exit (exitSuccess)
import qualified Debug.Trace as T
import Control.Applicative ((<$>), (<*>))
import qualified Control.Monad.State as ST
import Control.Monad (when, void)
import qualified Data.Vector as Vec
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Gfx as GFX
import qualified Gamgine.Math.Vect as V
import qualified Gamgine.Math.Matrix as M
import qualified Gamgine.Math.Utils as MU
import qualified Gamgine.Engine as EG
import Gamgine.Math.Vect
import Gamgine.Gfx ((<<<))
import Grid
import HsBot

gridWidth   = 10 :: Int
gridHeight  = 10 :: Int

dGridWidth  = fromIntegral gridWidth
dGridHeight = fromIntegral gridHeight

borderWidth = (max dGridWidth dGridHeight) / 8

ticksPerSecond = 4
maxFrameSkip   = 10
updateLoop     = EG.mkUpdateLoop ticksPerSecond maxFrameSkip executeRobots

io = ST.liftIO


main :: IO ()
main = do
   initGLFW
   hsBot <- mkDefaultHsBot gridWidth gridHeight
   time  <- GLFW.getTime
   ST.evalStateT (appLoop time) hsBot


appLoop :: Double -> HsBotST ()
appLoop nextFrame = do
   (nextFrame', _) <- updateLoop nextFrame

   pressed0 <- io $ GLFW.mouseButtonIsPressed GLFW.MouseButton0
   when pressed0 $ do
      coord <- io mousePosInGridCoords
      placeBlock coord

   pressed1 <- io $ GLFW.mouseButtonIsPressed GLFW.MouseButton1
   when pressed1 $ do
      coord <- io mousePosInGridCoords
      removeBlock coord

   render
   appLoop nextFrame'


render :: HsBotST ()
render = do
   clearScreen
   renderHsBot
   swapBuffers
   where
      swapBuffers = io GLFW.swapBuffers

      clearScreen = io $ do
         GL.glClearColor 0 0 0 0
         GL.glClear (fromIntegral GL.gl_COLOR_BUFFER_BIT)


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


mousePosInGridCoords :: IO GridCoord
mousePosInGridCoords = do
   mworld    <- mousePosInWorldCoords
   gridTrans <- gridTranslation
   let (x:.y:._) = mworld - gridTrans
   return (floor x:.floor y:.())


mousePosInWorldCoords :: IO V.Vect
mousePosInWorldCoords = do
   winDims  <- GLFW.getWindowDimensions
   mousePos <- GLFW.getMousePosition
   let winToWorldMtx = M.mkWinToWorldMatrix winDims (frustum winDims)
   return $ V.setElem 2 0 $ M.winToWorld winToWorldMtx mousePos


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
