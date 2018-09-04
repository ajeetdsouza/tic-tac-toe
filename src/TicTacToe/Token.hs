module TicTacToe.Token where

data Token = X | O deriving Eq

instance Show Token where
  show X = "×"
  show O = "○"

flipToken X = O
flipToken O = X
