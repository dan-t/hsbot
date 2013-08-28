{-# LANGUAGE TemplateHaskell #-}

module HsBot where
import Control.Lens
import Control.Applicative ((<$>), (<*>))
import qualified Control.Monad.State as ST
import qualified Data.List as L
import Grid
import Robot
import qualified Robot as R
import qualified Gamgine.Lens.State as LS
import Gamgine.Coroutine (runCoroutine)


data HsBotData = HsBotData {
   _grid :: Grid
   } deriving Show

makeLenses ''HsBotData

type HsBot = ST.StateT HsBotData IO


mkHsBot :: Int -> Int -> HsBotData
mkHsBot gridWidth gridHeight = HsBotData $ mkGrid gridWidth gridHeight


placeRobotRandomly :: Robot -> HsBot ()
placeRobotRandomly robot = do
   grid  <- use grid
   coord <- ST.liftIO $ randomAndFreeCoord grid
   placeRobot coord robot


placeRobot :: GridCoord -> Robot -> HsBot ()
placeRobot coord rob =
   grid . gridField coord . robot .= Just rob


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
   grid <- use grid
   ST.liftIO $ renderGrid grid
