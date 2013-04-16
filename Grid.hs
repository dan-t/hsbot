
module Grid where
#include "Gamgine/Utils.cpp"
import Control.Monad (when)
import qualified Data.Vector as Vec
import Data.Maybe (isJust, fromJust)
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
   coord :: GridCoord,
   robot :: Maybe R.Robot
   } deriving (Show, Eq)

type GridColumn = Vec.Vector GridField
type Grid       = Vec.Vector GridColumn


mkGrid :: Int -> Int -> Grid
mkGrid width height =
   Vec.generate width $ \x ->
      Vec.generate height $ \y ->
         GridField (x:.y:.()) Nothing


validCoord :: GridCoord -> Grid -> Bool
validCoord (x:.y:.()) grid =
   x >= 0 
      && x < gridWidth grid
      && y >= 0
      && y < gridHeight grid


toVec3d :: GridCoord -> V.Vec3 Double
toVec3d (x:.y:.()) = (fromIntegral x :. fromIntegral y :. 0)


getGridField :: GridCoord -> Grid -> GridField
getGridField c@(x:.y:.()) grid
   | validCoord c grid = (grid ! x) ! y
   | otherwise         = 
      error $ P.printf "Invalid gridCoord=%s for gridWidth=%d and gridHeight=%d" (show c) (gridWidth grid) (gridHeight grid)


setGridField :: GridCoord -> Grid -> GridField -> Grid
setGridField c@(x:.y:.()) grid field
   | validCoord c grid =
        let column  = grid ! x
            column' = Vec.unsafeUpd column [(y, field {coord = c})]
            grid'   = Vec.unsafeUpd grid [(x, column')]
            in grid'

   | otherwise =
      error $ P.printf "Invalid gridCoord=%s for gridWidth=%d and gridHeight=%d" (show c) (gridWidth grid) (gridHeight grid)


placeRobot :: GridCoord -> Grid -> R.Robot -> Grid
placeRobot coord grid robot = setGridField coord grid (GridField coord $ Just robot)


gridWidth :: Grid -> Int
gridWidth = Vec.length


gridHeight :: Grid -> Int
gridHeight grid
   | not $ Vec.null grid = Vec.length $ Vec.head grid
   | otherwise           = 0 


renderGrid :: Grid -> IO ()
renderGrid grid =
   Vec.forM_ grid $ \column ->
      Vec.forM_ column renderGridField


renderGridField :: GridField -> IO ()
renderGridField (GridField coord robot) = do
   Gfx.withPolyMode GL.gl_LINE $ do
      GL.glColor3f <<< ((1,1,1) :: Gfx.RGB)
      let minCoord = toVec3d coord
          maxCoord = minCoord + (1:.1:.0:.())
      Gfx.drawQuad minCoord maxCoord

   case robot of
        Just r -> renderRobot coord r
        _      -> return ()


renderRobot :: GridCoord -> R.Robot -> IO ()
renderRobot coord (R.Robot _ color) = do
   GL.glColor3f <<< color
   let minCoord = toVec3d coord + (0.2:.0.2:.0:.())
       maxCoord = minCoord + (0.6:.0.6:.0:.())
   Gfx.drawQuad minCoord maxCoord
