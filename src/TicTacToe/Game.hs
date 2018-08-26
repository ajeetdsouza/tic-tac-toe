module TicTacToe.Game where

import qualified TicTacToe.AI                  as AI
import qualified TicTacToe.Board               as Board
import qualified TicTacToe.User                as User

data Player = AI | Human deriving (Eq)

play :: Board.Board -> Char -> Player -> Player -> IO ()
play board token player1 player2 = do
  putStrLn . Board.showBoard $ board
  play' board token player1 player2
 where
  token' = Board.flipToken token

  play' board token player1 player2
    | Board.win board token' = putStrLn $ token' : " wins!"
    | Board.movesLeft board == 0 = putStrLn "It's a draw!"
    | otherwise = do
      board' <- Board.playMove board token <$> case player1 of
        Human -> User.readMove board token
        AI    -> do
          let move = AI.getMove board token
          putStrLn $ "Player move (" ++ [token] ++ "): " ++ Board.showMove move
          return move
      play board' token' player2 player1
