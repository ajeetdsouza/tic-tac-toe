module Main where

import qualified TicTacToe.Game                as Game
import qualified TicTacToe.Player              as Player

main :: IO ()
main = Game.new playerOptions
  where playerOptions = [Player.Human, Player.Minimax, Player.Random]
