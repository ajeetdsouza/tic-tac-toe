module Move where

import           Data.Char                      ( chr
                                                , ord
                                                )
import           Pretty

newtype Move = Move (Int, Int) deriving Show

instance Pretty Move where
  pretty (Move (x, y)) = x' ++ y'
   where
    x' = [chr (ord 'A' + x)]
    y' = show (1 + y)
