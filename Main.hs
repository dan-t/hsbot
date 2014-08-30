{-# LANGUAGE PatternGuards #-}

import System.Exit (exitSuccess)
import qualified Debug.Trace as T
import Control.Applicative ((<$>), (<*>))
import qualified Control.Monad.State as ST
import Control.Monad (when, void)
import Control.Arrow ((&&&))
import Control.Lens
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
   GLFW.init
   GLFW.windowHint $ GLFW.WindowHint'Resizable True
   GLFW.swapInterval 1
   Just win <- GLFW.createWindow 800 800 "" Nothing Nothing
   initCallbacks win
   GLFW.makeContextCurrent (Just win)

   hsBot     <- mkDefaultHsBot gridWidth gridHeight
   Just time <- GLFW.getTime
   ST.evalStateT (appLoop win time) hsBot


appLoop :: GLFW.Window -> Double -> HsBotST ()
appLoop win nextFrame = do
   io GLFW.pollEvents
   (nextFrame', _) <- updateLoop nextFrame

   pressed1 <- io $ isButtonPressed win GLFW.MouseButton'1
   when pressed1 $ do
      coord <- io (mousePosInGridCoords win)
      placeBlock coord

   pressed2 <- io $ isButtonPressed win GLFW.MouseButton'2
   when pressed2 $ do
      coord <- io (mousePosInGridCoords win)
      removeBlock coord

   render win
   appLoop win nextFrame'
   where
      isButtonPressed win button = (== GLFW.MouseButtonState'Pressed) <$> GLFW.getMouseButton win button


render :: GLFW.Window -> HsBotST ()
render win = do
   clearScreen
   renderHsBot
   swapBuffers
   where
      swapBuffers = io (GLFW.swapBuffers win)

      clearScreen = io $ do
         GL.glClearColor 0 0 0 0
         GL.glClear (fromIntegral GL.gl_COLOR_BUFFER_BIT)


initCallbacks :: GLFW.Window -> IO ()
initCallbacks win = do
   GLFW.setWindowSizeCallback win (Just resize)
   GLFW.setWindowCloseCallback win (Just quit)
   where
      resize win width height = do
         let M.Frustum {M.right = right, M.top = top} = frustum (width, height)

	 GL.glViewport 0 0 (fromIntegral width) (fromIntegral height)
	 GL.glMatrixMode GL.gl_PROJECTION
	 GL.glLoadIdentity
         GL.glOrtho 0 (GFX.floatToFloat right) 0 (GFX.floatToFloat top) (-1) 1

         GL.glMatrixMode GL.gl_MODELVIEW
         GL.glLoadIdentity
         gridTrans <- gridTranslation win
         GL.glTranslatef <<< gridTrans

      quit win = GLFW.destroyWindow win >> GLFW.terminate >> exitSuccess


mousePosInGridCoords :: GLFW.Window -> IO GridCoord
mousePosInGridCoords win = do
   mworld    <- mousePosInWorldCoords win
   gridTrans <- gridTranslation win
   let (x:.y:._) = mworld - gridTrans
   return (floor x:.floor y:.())


mousePosInWorldCoords :: GLFW.Window -> IO V.Vect
mousePosInWorldCoords win = do
   winDims  <- GLFW.getWindowSize win
   mousePos <- GLFW.getCursorPos win
   let winToWorldMtx = M.mkWinToWorldMatrix winDims (frustum winDims)
   return $ V.setElem 2 0 $ M.winToWorld winToWorldMtx (mousePos & each %~ floor)


gridTranslation :: GLFW.Window -> IO V.Vect
gridTranslation win = do
   M.Frustum {M.right = r, M.top = t} <- frustum <$> GLFW.getWindowSize win
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
