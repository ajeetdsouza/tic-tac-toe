module TicTacToe.Move where

import           Data.Char                      ( chr
                                                , ord
                                                )

newtype Move = Move (Int, Int)

instance Show Move where
  show (Move (x, y)) = x' ++ y'
   where
    x' = [chr (ord 'A' + x)]
    y' = show (1 + y)
