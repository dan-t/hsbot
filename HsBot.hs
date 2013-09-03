{-# LANGUAGE TemplateHaskell, PatternGuards #-}

module HsBot where
import Control.Lens hiding (Action)
import Control.Applicative ((<$>), (<*>))
import qualified Text.Printf as P
import qualified Control.Monad.State as ST
import qualified Data.List as L
import Grid
import Robot
import qualified Robot as R


type IdWithAction      = (RobotId, Action)
type IdsWithActions    = [IdWithAction]
type IdWithResult      = (RobotId, ActionResult)
type IdsWithResults    = [IdWithResult]
type RobotsWithResults = [(Robot, ActionResult)]


data HsBot = HsBot {
   _grid          :: Grid,
   _actionResults :: IdsWithResults
   } deriving Show

makeLenses ''HsBot

type HsBotST = ST.StateT HsBot IO


mkHsBot :: Int -> Int -> HsBot
mkHsBot gridWidth gridHeight = HsBot (mkGrid gridWidth gridHeight) []


mkDefaultHsBot :: Int -> Int -> IO HsBot
mkDefaultHsBot gridWidth gridHeight = 
   placeRobotRandomly robot1 hsBot >>= placeRobotRandomly robot2
   where
      hsBot  = mkHsBot gridWidth gridHeight
      robot1 = defaultRobot 1 (1, 0, 0)
      robot2 = defaultRobot 2 (0, 1, 0)


placeRobotRandomly :: Robot -> HsBot -> IO HsBot
placeRobotRandomly robot hsBot = do
   coord <- randomAndFreeCoord $ hsBot ^. grid
   return $ placeRobot coord robot hsBot


placeRobot :: GridCoord -> Robot -> HsBot -> HsBot
placeRobot coord rob hsBot = hsBot & grid . atCoord coord . robot .~ Just rob


executeRobots :: HsBotST ()
executeRobots = ST.modify execRobots


execRobots :: HsBot -> HsBot
execRobots hsBot = apply (actions results) hsBot
   where
      results = matchRobotsWithResults hsBot


apply :: IdsWithActions -> HsBot -> HsBot
apply actions hsBot = hsBot' & actionResults .~ results  
   where
      (hsBot', results) = L.foldl' applyAct (hsBot, []) actions
         where
            applyAct (hsBot, res) act =
               let (hsBot', res') = applyAction act hsBot
                   in (hsBot', res' : res)


applyAction :: IdWithAction -> HsBot -> (HsBot, IdWithResult)
applyAction (id, NoAction) hsBot = (hsBot, (id, NoResult))
applyAction (id, Move dir) hsBot
   | Just grid' <- moveRobotAlongDir id dir (hsBot ^. grid) = (hsBot & grid .~ grid', (id, Moved dir True))
   | otherwise                                              = (hsBot, (id, Moved dir False))


actions :: RobotsWithResults -> IdsWithActions
actions = L.map $ \(rob, res) -> (rob ^. robotId, rob ^. execute $ res)


matchRobotsWithResults :: HsBot -> RobotsWithResults
matchRobotsWithResults hsBot =
   L.map (\rob -> case lookup (rob ^. robotId) results of
                       Just res -> (rob, res)
                       _        -> (rob, NoResult))
         robs
   where
      robs    = hsBot ^. grid . to robots
      results = hsBot ^. actionResults


renderHsBot :: HsBotST ()
renderHsBot = do
   grid <- use grid
   ST.liftIO $ renderGrid grid
