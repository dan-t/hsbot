
module Robot where
import qualified Gamgine.Gfx as Gfx

data Robot = Robot {
   id    :: Int,
   color :: Gfx.RGB
   } deriving (Show, Eq)
