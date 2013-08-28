{-# LANGUAGE TemplateHaskell #-}

module Robot where
import Control.Lens hiding (Action)
import qualified Gamgine.Gfx as Gfx
import Gamgine.Coroutine (Coroutine(..))

type Id = Int

data Direction = PlusX | MinusX | PlusY | MinusY deriving (Show, Eq, Enum)

data Action = Move Direction
            | NoAction
            deriving (Show, Eq)

data ActionResult = Moved Direction Bool
                  | NoResult
                  deriving (Show, Eq)

data Robot = Robot {
   _robotId :: Id,
   _color   :: Gfx.RGB,
   _execute :: Coroutine ActionResult Action
   }

makeLenses ''Robot

instance Show Robot where
   show robot = "defaultRobot" ++ show (robot ^. robotId) ++ show (robot ^. color)

instance Eq Robot where
   r1 == r2 = r1 ^. robotId == r2 ^. robotId


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
