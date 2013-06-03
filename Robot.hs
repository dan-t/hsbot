
module Robot where
import qualified Gamgine.Gfx as Gfx

type Id = Int

data Robot = Robot {
   robotId :: Id,
   color   :: Gfx.RGB,
   execute :: ActionResult -> Action
   }

instance Show Robot where
   show (Robot id color _) = "defaultRobot" ++ show id ++ show color

instance Eq Robot where
   r1 == r2 = robotId r1 == robotId r2


defaultRobot :: Id -> Gfx.RGB -> Robot
defaultRobot id color = Robot id color exec
   where
      exec NoResult          = Move PlusY
      exec (Moved dir True ) = Move dir
      exec (Moved dir False) = Move $ nextDir dir

      nextDir PlusY  = PlusX
      nextDir PlusX  = MinusY
      nextDir MinusY = MinusX
      nextDir MinusX = PlusY


data Direction = PlusX | MinusX | PlusY | MinusY deriving (Show, Eq, Enum)

data Action = Move Direction deriving (Show, Eq)

data ActionResult = Moved Direction Bool
                  | NoResult
                  deriving (Show, Eq)
