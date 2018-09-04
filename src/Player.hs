module Player where

import           Board
import           Move
import           Token
import qualified Player.Human
import qualified Player.Minimax
import qualified Player.Random

data Player = Minimax | Random | Human

instance Show Player where
  show Minimax = "Minimax"
  show Random  = "Random"
  show Human   = "Human"

getMove :: Player -> Board -> Token -> IO Move
getMove Minimax = Player.Minimax.getMove
getMove Random  = Player.Random.getMove
getMove Human   = Player.Human.getMove
