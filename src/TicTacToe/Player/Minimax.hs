module TicTacToe.Player.Minimax
  ( getMove
  )
where

import           Data.List                      ( maximumBy )
import           Data.Maybe                     ( fromJust )
import           Data.Ord                       ( comparing )

import           TicTacToe.Board
import           TicTacToe.Move
import           TicTacToe.Token

score :: Board -> Token -> Bool -> Int
score board token player
  | isWinner board token' = (if player then negate else id) (1 + length moves)
  | isFull board          = 0
  | otherwise             = (if player then maximum else minimum) scores
 where
  token'  = flipToken token
  player' = not player

  moves   = validMoves board
  states  = map (fromJust . playMove board token) moves
  scores  = map (\board' -> score board' token' player') states

getMove :: Board -> Token -> IO Move
getMove board token = return . fst . maximumBy (comparing snd) $ zip moves scores
 where
  token' = flipToken token

  moves  = validMoves board
  states = map (fromJust . playMove board token) moves
  scores = map (\board' -> score board' token' False) states
