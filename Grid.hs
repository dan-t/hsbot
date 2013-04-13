
module Grid where
#include "Gamgine/Utils.cpp"
import qualified Control.Monad as CM
import qualified Data.Vector as Vec
import qualified Text.Printf as P
import Data.Vector ((!))
import qualified Data.Vec as V
import Data.Vec ((:.)(..))
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Gfx as Gfx
import Gamgine.Gfx ((<<<))
import qualified Robot as R
IMPORT_LENS_AS_LE


type GridCoord = V.Vec2 Int

data GridField = GridField {
   robot :: Maybe R.Robot
   } deriving (Show, Eq)

type GridColumn = Vec.Vector GridField
type Grid       = Vec.Vector GridColumn


validCoord :: GridCoord -> Grid -> Bool
validCoord (x:.y:.()) grid =
   x >= 0 
      && x < gridWidth grid
      && y >= 0
      && y < gridHeight grid


getGridField :: GridCoord -> Grid -> GridField
getGridField c@(x:.y:.()) grid
   | validCoord c grid = (grid ! y) ! x
   | otherwise         = 
      error $ P.printf "Invalid gridCoord=%s for gridWidth=%d and gridHeight=%d" (show c) (gridWidth grid) (gridHeight grid)


setGridField :: GridCoord -> Grid -> GridField -> Grid
setGridField c@(x:.y:.()) grid field
   | validCoord c grid =
        let column  = grid ! y
            column' = column // [(x, field)]
            in grid // [(y, column')] 

   | otherwise =
      error $ P.printf "Invalid gridCoord=%s for gridWidth=%d and gridHeight=%d" (show c) (gridWidth grid) (gridHeight grid)









gridWidth :: Grid -> Int
gridWidth = Vec.length


gridHeight :: Grid -> Int
gridHeight grid
   | not $ Vec.null grid = Vec.length $ Vec.head grid
   | otherwise           = 0 


mkGrid :: Int -> Int -> Grid
mkGrid width height =
   Vec.generate width $ \x ->
      Vec.generate height $ \y ->
         GridField Nothing


--placeRobot :: Grid -> GridCoord -> Robot -> Maybe Grid
--placeRobot grid (x:.y:.()) robot
--     let gfield = (grid ! y) ! x
--
--
--
--   | otherwise = Nothing
--


renderGrid :: Grid -> IO ()
renderGrid grid = do
   GL.glColor3f <<< ((1,1,1) :: Gfx.RGB)

   -- render columns
   CM.forM_ [0 .. width - 1] $ \column ->
      Gfx.withPrimitive GL.gl_LINE_STRIP $
         CM.forM_ [0 .. height - 1] $ \row ->
            GL.glVertex3f (fromIntegral column) (fromIntegral row) 0

   -- render rows
   CM.forM_ [0 .. height - 1] $ \row ->
      Gfx.withPrimitive GL.gl_LINE_STRIP $
         CM.forM_ [0 .. width - 1] $ \column ->
            GL.glVertex3f (fromIntegral column) (fromIntegral row) 0

   where
      width  = gridWidth grid
      height = gridHeight grid


--renderRobot :: GridCoord -> Robot -> IO ()
--renderRobot (x:.y:.()) robot
