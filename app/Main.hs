module Main where

import qualified Game
import qualified Player

main :: IO ()
main = Game.new playerOptions
  where playerOptions = [Player.Human, Player.Minimax, Player.Random]
