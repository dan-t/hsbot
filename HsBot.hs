
module HsBot where
#include "Gamgine/Utils.cpp"
IMPORT_LENS_AS_LE

import Control.Applicative ((<$>), (<*>))
import qualified Control.Monad.State as ST
import qualified Data.List as L
import qualified Grid as G
import qualified Robot as R
import qualified Gamgine.Lens.State as LS
import Gamgine.Coroutine (runCoroutine)


data HsBotData = HsBotData {
   grid :: G.Grid
   } deriving Show

LENS(grid)

type HsBot = ST.StateT HsBotData IO


mkHsBot :: Int -> Int -> HsBotData
mkHsBot gridWidth gridHeight = HsBotData $ G.mkGrid gridWidth gridHeight


placeRobotRandomly :: R.Robot -> HsBot ()
placeRobotRandomly robot = do
   grid  <- LS.getL gridL
   coord <- ST.liftIO $ G.randomAndFreeCoord grid
   placeRobot coord robot


placeRobot :: G.GridCoord -> R.Robot -> HsBot ()
placeRobot coord robot = ST.modify $ LE.setL (G.robotL . G.gridFieldL coord . gridL) (Just robot)


type Actions       = [(R.Id, R.Action)]
type ActionResults = [(R.Id, R.ActionResult)]


--executeRobots :: ActionResults -> HsBot Actions
--executeRobots results = (grid', actions)
--   where
--      grid' = L.foldl' setExec grid execs'
--         where
--            setExec grid (coord, exec') = LE.modL (G.robotL . G.gridFieldL coord) (\robot ->
--               case robot of
--                    Just r -> Just r {R.execute = exec'}
--                    _      -> robot
--               ) grid
--
--      (execs', actions) = L.unzip $ L.map execRobot (G.robots grid)
--         where
--            execRobot (coord, R.Robot {R.robotId = id, R.execute = exec}) =
--               let (action, exec') = runCoroutine exec (result id)
--                   in ((coord, exec'), (id, action))
--
--      result id | Just res <- lookup id results = res
--                | otherwise                     = R.NoResult


renderHsBot :: HsBot ()
renderHsBot = do
   grid <- LS.getL gridL
   ST.liftIO $ G.renderGrid grid
