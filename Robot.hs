{-# LANGUAGE TemplateHaskell #-}

module Robot where
import Control.Lens hiding (Action)
import qualified Text.Printf as P
import qualified Gamgine.Gfx as Gfx

type RobotId = Int

data Direction = PlusX | MinusX | PlusY | MinusY deriving (Show, Eq, Enum)

data Action = Move Direction
            | NoAction
            deriving (Show, Eq)

data ActionResult = Moved Direction Bool
                  | NoResult
                  deriving (Show, Eq)

data Robot = Robot {
   _robotId :: RobotId,
   _color   :: Gfx.RGB,
   _execute :: (ActionResult -> Action)
   }

makeLenses ''Robot

instance Show Robot where
   show robot = P.printf "defaultRobot %i %s" (robot ^. robotId) (show $ robot ^. color)

instance Eq Robot where
   r1 == r2 = r1 ^. robotId == r2 ^. robotId


defaultRobot :: RobotId -> Gfx.RGB -> Robot
defaultRobot id color = Robot id color exec
   where
      exec NoResult          = Move PlusY
      exec (Moved dir True ) = Move dir
      exec (Moved dir False) = Move $ nextDir dir

      nextDir PlusY  = PlusX
      nextDir PlusX  = MinusY
      nextDir MinusY = MinusX
      nextDir MinusX = PlusY
