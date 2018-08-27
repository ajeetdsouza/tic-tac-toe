module TicTacToe.Token where

import           Data.Char                      ( toLower )

data Token = X | O deriving (Eq)

instance Show Token where
    show X = "×"
    show O = "○"

instance Read Token where
    readsPrec _ [token] = case toLower token of
        'x' -> [(X, "")]
        'o' -> [(O, "")]
        _   -> []

    readsPrec _ _ = []

other :: Token -> Token
other X = O
other O = X
