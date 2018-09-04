module Player where

import           Board
import           Move
import           Token
import qualified Player.Human
import qualified Player.Minimax
import qualified Player.Random
import           Pretty

data Player = Minimax | Random | Human deriving Show

instance Pretty Player where
  pretty Minimax = "Minimax"
  pretty Random  = "Random"
  pretty Human   = "Human"

getMove :: Player -> Board -> Token -> IO Move
getMove Minimax = Player.Minimax.getMove
getMove Random  = Player.Random.getMove
getMove Human   = Player.Human.getMove
