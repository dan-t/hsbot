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
   ifoldlM addRobot (mkHsBot gridWidth gridHeight) colors
   where
      addRobot id hsBot color = placeRobotRandomly (defaultRobot id color) hsBot
      colors = [(r, g, b) | r <- [1, 0, 0], g <- [0, 1, 0], b <- [0, 0, 1]]


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
apply actions hsBot = L.foldl' (flip applyAction) (hsBot & actionResults .~ []) actions
   where
      applyAction (id, NoAction) hsBot = hsBot
      applyAction (id, Move dir) hsBot =
         case moveRobotAlongDir id dir (hsBot ^. grid) of
              Just grid' -> hsBot & grid .~ grid'
                                  & actionResults %~ ((id, Moved dir True) :)
              _          -> hsBot & actionResults %~ ((id, Moved dir False) :)


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
