{-# LANGUAGE PatternGuards #-}

module Grid where
#include "Gamgine/Utils.cpp"
import System.Random (randomRIO)
import Control.Monad (when)
import qualified Data.List as L
import qualified Data.Vector as Vec
import Data.Maybe (isJust, fromJust)
import qualified Text.Printf as P
import Data.Vector ((!))
import qualified Data.Vec as V
import Data.Vec ((:.)(..))
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Gfx as Gfx
import Gamgine.Gfx ((<<<))
import Gamgine.Coroutine (Coroutine, runCoroutine)
import qualified Robot as R
IMPORT_LENS_AS_LE


type GridCoord = V.Vec2 Int

data GridField = GridField {
   coord :: GridCoord,
   robot :: Maybe R.Robot
   } deriving (Show, Eq)

LENS(coord)
LENS(robot)

type GridColumn = Vec.Vector GridField
type Grid       = Vec.Vector GridColumn


mkGrid :: Int -> Int -> Grid
mkGrid width height =
   Vec.generate width $ \x ->
      Vec.generate height $ \y ->
         GridField (x:.y:.()) Nothing


forM_ :: Grid -> (GridField -> IO ()) -> IO ()
forM_ grid f = Vec.forM_ grid $ \column -> Vec.forM_ column f


foldl' :: (a -> GridField -> a) -> a -> Grid -> a
foldl' f a grid = Vec.foldl' (\a column -> Vec.foldl' f a column) a grid


find :: (GridField -> Bool) -> Grid -> [GridField]
find f grid = foldl' g [] grid
   where
      g fields field
         | f field   = field : fields
         | otherwise = fields


randomAndFreeCoord :: Grid -> IO GridCoord
randomAndFreeCoord grid = do
   x <- randomRIO (0, gridWidth grid - 1)
   y <- randomRIO (0, gridHeight grid - 1)
   let coord = x:.y:.()
   case getGridField coord grid of
        GridField _ Nothing -> return coord
        _                   -> randomAndFreeCoord grid


validCoord :: GridCoord -> Grid -> Bool
validCoord (x:.y:.()) grid =
   x >= 0 
      && x < gridWidth grid
      && y >= 0
      && y < gridHeight grid


constrainCoord :: GridCoord -> Grid -> GridCoord
constrainCoord (x:.y:.()) grid = (x':.y':.())
   where
      x' = min (gridWidth grid - 1) (max 0 x)
      y' = min (gridHeight grid - 1) (max 0 y)


addDir :: GridCoord -> R.Direction -> GridCoord
(x:.y:.()) `addDir` dir =
  case dir of
       R.PlusY  -> x:.(y + 1):.()
       R.MinusY -> x:.(y - 1):.()
       R.MinusX -> (x - 1):.y:.()
       R.PlusX  -> (x + 1):.y:.()


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


gridFieldL :: GridCoord -> LE.Lens Grid GridField
gridFieldL coord = LE.lens (getGridField coord) (\field grid -> setGridField coord grid field)


gridWidth :: Grid -> Int
gridWidth = Vec.length


gridHeight :: Grid -> Int
gridHeight grid
   | not $ Vec.null grid = Vec.length $ Vec.head grid
   | otherwise           = 0 


renderGrid :: Grid -> IO ()
renderGrid grid = forM_ grid renderGridField


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
renderRobot coord (R.Robot {R.color = color}) = do
   GL.glColor3f <<< color
   let minCoord = toVec3d coord + (0.2:.0.2:.0:.())
       maxCoord = minCoord + (0.6:.0.6:.0:.())
   Gfx.drawQuad minCoord maxCoord


robots :: Grid -> [(GridCoord, R.Robot)]
robots grid = foldl' f [] grid
   where
      f robots (GridField coord (Just robot)) = (coord, robot) : robots
      f robots (GridField _     _           ) = robots
