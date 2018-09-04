module TicTacToe.Player where

import           TicTacToe.Board
import           TicTacToe.Move
import           TicTacToe.Token
import qualified TicTacToe.Player.Human
import qualified TicTacToe.Player.Minimax
import qualified TicTacToe.Player.Random

data Player = Minimax | Random | Human

instance Show Player where
  show Minimax = "Minimax"
  show Random  = "Random"
  show Human   = "Human"

getMove :: Player -> Board -> Token -> IO Move
getMove Minimax = TicTacToe.Player.Minimax.getMove
getMove Random  = TicTacToe.Player.Random.getMove
getMove Human   = TicTacToe.Player.Human.getMove
