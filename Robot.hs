
module Robot where
#include "Gamgine/Utils.cpp"
import qualified Gamgine.Gfx as Gfx
import Gamgine.Coroutine (Coroutine(..))
IMPORT_LENS_AS_LE

type Id = Int

data Robot = Robot {
   robotId :: Id,
   color   :: Gfx.RGB,
   execute :: Coroutine ActionResult Action
   }

LENS(robotId)
LENS(color)
LENS(execute)

instance Show Robot where
   show (Robot id color _) = "defaultRobot" ++ show id ++ show color

instance Eq Robot where
   r1 == r2 = robotId r1 == robotId r2


defaultRobot :: Id -> Gfx.RGB -> Robot
defaultRobot id color = Robot id color (Coroutine exec)
   where
      exec NoResult          = (Move PlusY, Coroutine exec)
      exec (Moved dir True ) = (Move dir, Coroutine exec)
      exec (Moved dir False) = (Move $ nextDir dir, Coroutine exec)

      nextDir PlusY  = PlusX
      nextDir PlusX  = MinusY
      nextDir MinusY = MinusX
      nextDir MinusX = PlusY


data Direction = PlusX | MinusX | PlusY | MinusY deriving (Show, Eq, Enum)

data Action = Move Direction
            | NoAction
            deriving (Show, Eq)

data ActionResult = Moved Direction Bool
                  | NoResult
                  deriving (Show, Eq)
