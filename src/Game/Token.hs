module Game.Token where

import           Pretty

data Token = X | O deriving (Eq, Show)

instance Pretty Token where
  pretty X = "×"
  pretty O = "○"

flipToken X = O
flipToken O = X
