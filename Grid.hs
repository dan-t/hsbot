{-# LANGUAGE PatternGuards, TemplateHaskell, Rank2Types #-}

module Grid where
import System.Random (randomRIO)
import Control.Monad (when)
import Control.Lens
import qualified Data.List as L
import qualified Data.Vector as Vec
import Data.Maybe (isJust, fromJust)
import qualified Text.Printf as P
import Data.Vector ((!), (//))
import qualified Data.Vec as V
import Data.Vec ((:.)(..))
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Gfx as Gfx
import Gamgine.Gfx ((<<<))
import Gamgine.Coroutine (Coroutine, runCoroutine)
import Robot
import qualified Robot as R


type GridCoord = V.Vec2 Int

data GridField = GridField {
   _coord :: GridCoord,
   _robot :: Maybe Robot
   } deriving (Show, Eq)

makeLenses ''GridField

type GridFields = Vec.Vector GridField

data Grid = Grid {
   _width  :: Int,
   _height :: Int,
   _fields :: GridFields
   } deriving (Show)

makeLenses ''Grid


mkGrid :: Int -> Int -> Grid
mkGrid width height = Grid width height mkFields
   where
      mkFields = Vec.generate (width * height) $ \i ->
         let (x, y) = i `quotRem` width
             in GridField (x:.y:.()) Nothing


forM_ :: Grid -> (GridField -> IO ()) -> IO ()
forM_ grid = Vec.forM_ $ grid ^. fields


foldl' :: (a -> GridField -> a) -> a -> Grid -> a
foldl' f a grid = Vec.foldl' f a $ grid ^. fields


find :: (GridField -> Bool) -> Grid -> [GridField]
find f grid = foldl' g [] grid
   where
      g fields field
         | f field   = field : fields
         | otherwise = fields


randomAndFreeCoord :: Grid -> IO GridCoord
randomAndFreeCoord grid = do
   x <- randomRIO (0, grid ^. width - 1)
   y <- randomRIO (0, grid ^. height - 1)
   let coord = x:.y:.()
   case getGridField coord grid of
        GridField _ Nothing -> return coord
        _                   -> randomAndFreeCoord grid


validCoord :: GridCoord -> Grid -> Bool
validCoord (x:.y:.()) grid =
   x >= 0 
      && x < grid ^. width
      && y >= 0
      && y < grid ^. height


constrainCoord :: GridCoord -> Grid -> GridCoord
constrainCoord (x:.y:.()) grid = (x':.y':.())
   where
      x' = min (grid ^. width - 1) (max 0 x)
      y' = min (grid ^. height - 1) (max 0 y)


addDir :: GridCoord -> Direction -> GridCoord
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
   | validCoord c grid = grid ^. fields . atIndex (x * y)
   | otherwise         = 
      error $ P.printf "Invalid gridCoord=%s for gridWidth=%d and gridHeight=%d" (show c) (grid ^. width) (grid ^. height)


setGridField :: GridCoord -> Grid -> GridField -> Grid
setGridField c@(x:.y:.()) grid field
   | validCoord c grid = grid & fields . atIndex (x * y) .~ field
   | otherwise =
      error $ P.printf "Invalid gridCoord=%s for gridWidth=%d and gridHeight=%d" (show c) (grid ^. width) (grid ^. height)


atCoord :: GridCoord -> Lens' Grid GridField
atCoord coord = lens (getGridField coord) (\grid field -> setGridField coord grid field)


atIndex :: Int -> Lens' GridFields GridField
atIndex i = lens (! i) (\gfs gf -> gfs // [(i, gf)])


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


renderRobot :: GridCoord -> Robot -> IO ()
renderRobot coord robot = do
   GL.glColor3f <<< robot ^. color
   let minCoord = toVec3d coord + (0.2:.0.2:.0:.())
       maxCoord = minCoord + (0.6:.0.6:.0:.())
   Gfx.drawQuad minCoord maxCoord


robots :: Grid -> [(GridCoord, Robot)]
robots grid = foldl' f [] grid
   where
      f robots (GridField coord (Just robot)) = (coord, robot) : robots
      f robots (GridField _     _           ) = robots
