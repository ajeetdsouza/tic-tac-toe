module TicTacToe.Move where

import           Data.Char                      ( chr
                                                , ord
                                                , toLower
                                                )

data Move = Move Int Int

instance Show Move where
    show (Move x y) = chr (ord 'A' + x) : show (1 + y)

instance Read Move where
    readsPrec _ [x, y] = case mkMove x' y' of
            Left _ -> []
            Right move -> [(move, "")]
        where
            x' = (ord . toLower) x - ord 'a'
            y' = ord y - ord '1'

    readsPrec _ _ = []

mkMove :: Int -> Int -> Either String Move
mkMove x y
  | 0 <= x && x <= 2 && 0 <= y && y <= 2 = Right $ Move x y
  | otherwise =  Left
  $  "Move out of bounds: ("
  ++ show x
  ++ ", "
  ++ show y
  ++ ")"
