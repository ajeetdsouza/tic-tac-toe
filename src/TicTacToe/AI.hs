module TicTacToe.AI where

import           Data.Either                    ( fromRight )
import           Data.List                      ( maximumBy )
import           Data.Ord                       ( comparing )

import           TicTacToe.Board               as Board
import           TicTacToe.Move                as Move
import           TicTacToe.Token               as Token

score :: Board.Board -> Token.Token -> Bool -> Int
score board token player
  | Board.isWinner board token' = (if player then negate else id)
    (1 + length moves)
  | Board.isFull board = 0
  | otherwise = (if player then maximum else minimum) scores
 where
  token'  = Token.other token
  player' = not player

  moves   = Board.validMoves board
  states  = map
    (fromRight (error "Invalid move generated") . Board.playMove board token)
    moves
  scores = map (\board' -> score board' token' player') states

getMove :: Board.Board -> Token.Token -> Move.Move
getMove board token = fst . maximumBy (comparing snd) $ zip moves scores
 where
  token' = Token.other token
  moves  = Board.validMoves board
  states = map
    (fromRight (error "Invalid move generated") . Board.playMove board token)
    moves
  scores = map (\board' -> score board' token' False) states
