{-# LANGUAGE PatternGuards, TemplateHaskell, Rank2Types, LambdaCase #-}

module Grid where
import System.Random (randomRIO)
import Control.Monad (when)
import Control.Lens hiding (index)
import Control.Arrow ((&&&))
import qualified Data.List as L
import qualified Data.Vector as Vec
import Data.Maybe (isJust, fromJust, isNothing)
import qualified Text.Printf as P
import Data.Vector ((!), (//))
import qualified Data.Vec as V
import Data.Vec ((:.)(..))
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Gfx as Gfx
import Gamgine.Gfx ((<<<))
import Robot
import qualified Robot as R

type GridCoord = V.Vec2 Int

mkCoord :: Int -> Int -> V.Vec2 Int
mkCoord x y = x:.y:.()


data GridField = GridField {
   _coord :: GridCoord,
   _robot :: Maybe Robot
   } deriving (Show, Eq)

makeLenses ''GridField


justRobot :: Lens' GridField Robot
justRobot = lens (^. robot . to fromJust) (\gf r -> gf & robot .~ Just r)


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
         let (x, y) = i `quotRem` height
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
   if isFree coord grid
      then return coord
      else randomAndFreeCoord grid


isFree :: GridCoord -> Grid -> Bool
isFree coord grid = grid ^. atCoord coord . robot . to isNothing


isValid :: GridCoord -> Grid -> Bool
isValid (x:.y:.()) grid =
   x >= 0 
      && x < grid ^. width
      && y >= 0
      && y < grid ^. height


addDir :: GridCoord -> Direction -> Grid -> Maybe GridCoord
addDir (x:.y:.()) dir grid
   | isValid newCoord grid && isFree newCoord grid = Just newCoord
   | otherwise                                     = Nothing
   where
      newCoord = case dir of
                      R.PlusY  -> x:.(y + 1):.()
                      R.MinusY -> x:.(y - 1):.()
                      R.MinusX -> (x - 1):.y:.()
                      R.PlusX  -> (x + 1):.y:.()


toVec3d :: GridCoord -> V.Vec3 Double
toVec3d (x:.y:.()) = (fromIntegral x :. fromIntegral y :. 0)


index :: GridCoord -> Grid -> Int
index (x:.y:.()) grid = (x * grid ^. height) + y

getGridField :: GridCoord -> Grid -> GridField
getGridField coord@(x:.y:.()) grid
   | isValid coord grid = grid ^. fields . atIndex (index coord grid)
   | otherwise          = invalidCoordError coord grid 


setGridField :: GridCoord -> Grid -> GridField -> Grid
setGridField coord@(x:.y:.()) grid field
   | isValid coord grid = grid & fields . atIndex (index coord grid) .~ field
   | otherwise          = invalidCoordError coord grid


invalidCoordError :: GridCoord -> Grid -> a
invalidCoordError coord grid =
   error $ P.printf "Invalid gridCoord=%s for gridWidth=%d and gridHeight=%d" (show coord) (grid ^. width) (grid ^. height)


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


havingRobot :: Traversal' GridField GridField
havingRobot = filtered (^. robot . to isJust)


withRobot :: RobotId -> Traversal' GridField GridField
withRobot id = filtered (^. robot . to (\case {Just (Robot rid _ _) -> id == rid; _ -> False}))


robots :: Grid -> [Robot]
robots grid = grid ^.. fields . traversed . havingRobot . justRobot


coordOf :: RobotId -> Grid -> Maybe GridCoord
coordOf id grid
   | [coord] <- grid ^.. fields . traversed . withRobot id . coord = Just coord
   | otherwise                                                     = Nothing


moveRobotAlongDir :: RobotId -> Direction -> Grid -> Maybe Grid
moveRobotAlongDir id dir grid = do
   oldCoord <- coordOf id grid
   rob      <- grid ^. atCoord oldCoord . robot
   newCoord <- addDir oldCoord dir grid
   return $ grid & atCoord oldCoord . robot .~ Nothing
                 & atCoord newCoord . robot .~ Just rob
