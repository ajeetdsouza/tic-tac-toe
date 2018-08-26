module App.Game where

import qualified App.AI                        as AI
import qualified App.Board                     as Board
import qualified App.User                      as User
import           Data.Char                      ( chr
                                                , ord
                                                )

data Player = AI | Human deriving (Eq)

play :: Board.Board -> Char -> Player -> Player -> IO ()
play board token player1 player2 = do
  Board.display board
  play' board token player1 player2
 where
  token' = Board.flipToken token
  cpuMove move = chr (ord 'a' + fst move) : show (1 + snd move)

  play' board token player1 player2
    | Board.win board token' = putStrLn $ token' : " wins!"
    | Board.movesLeft board == 0 = putStrLn "It's a draw!"
    | otherwise = do
      board' <- Board.move board token <$> case player1 of
        Human -> User.readMove board token
        AI    -> do
          let move = AI.getMove board token
          putStrLn $ "CPU move (" ++ [token] ++ "): " ++ cpuMove move
          return move
      play board' token' player2 player1
