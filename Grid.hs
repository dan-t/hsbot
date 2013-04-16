
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


mkGrid :: Int -> Int -> Grid
mkGrid width height =
   Vec.generate width $ \x ->
      Vec.generate height $ \y ->
         GridField Nothing


validCoord :: GridCoord -> Grid -> Bool
validCoord (x:.y:.()) grid =
   x >= 0 
      && x < gridWidth grid
      && y >= 0
      && y < gridHeight grid


getGridField :: GridCoord -> Grid -> GridField
getGridField c@(x:.y:.()) grid
   | validCoord c grid = (grid ! x) ! y
   | otherwise         = 
      error $ P.printf "Invalid gridCoord=%s for gridWidth=%d and gridHeight=%d" (show c) (gridWidth grid) (gridHeight grid)


setGridField :: GridCoord -> Grid -> GridField -> Grid
setGridField c@(x:.y:.()) grid field
   | validCoord c grid =
        let column  = grid ! x
            column' = Vec.unsafeUpd column [(y, field)]
            grid'   = Vec.unsafeUpd grid [(x, column')]
            in grid'

   | otherwise =
      error $ P.printf "Invalid gridCoord=%s for gridWidth=%d and gridHeight=%d" (show c) (gridWidth grid) (gridHeight grid)


placeRobot :: GridCoord -> Grid -> R.Robot -> Grid
placeRobot coord grid robot = setGridField coord grid (GridField $ Just robot)


gridWidth :: Grid -> Int
gridWidth = Vec.length


gridHeight :: Grid -> Int
gridHeight grid
   | not $ Vec.null grid = Vec.length $ Vec.head grid
   | otherwise           = 0 


renderGrid :: Grid -> IO ()
renderGrid grid = do
   GL.glColor3f <<< ((1,1,1) :: Gfx.RGB)

   -- render columns
   CM.forM_ [0 .. width] $ \column ->
      Gfx.withPrimitive GL.gl_LINE_STRIP $
         CM.forM_ [0 .. height] $ \row ->
            GL.glVertex3f (fromIntegral column) (fromIntegral row) 0

   -- render rows
   CM.forM_ [0 .. height] $ \row ->
      Gfx.withPrimitive GL.gl_LINE_STRIP $
         CM.forM_ [0 .. width] $ \column ->
            GL.glVertex3f (fromIntegral column) (fromIntegral row) 0

   where
      width  = gridWidth grid
      height = gridHeight grid
