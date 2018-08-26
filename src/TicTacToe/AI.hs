module TicTacToe.AI where

import           Data.List                      ( maximumBy )
import           Data.Ord                       ( comparing )

import qualified TicTacToe.Board               as Board

score :: Board.Board -> Char -> Bool -> Int -> Int
score board token player depth
  | Board.win board token' = (if player then negate else id) (1 + movesLeft)
  | movesLeft == 0         = 0
  | otherwise              = (if player then maximum else minimum) scores
 where
  token'    = Board.flipToken token
  player'   = not player
  depth'    = depth + 1

  moves     = Board.possibleMoves board
  movesLeft = Board.movesLeft board
  states    = map (Board.playMove board token) moves
  scores    = map (\board' -> score board' token' player' depth') states

getMove :: Board.Board -> Char -> Board.Move
getMove board token = fst . maximumBy (comparing snd) $ zip moves scores
 where
  token' = Board.flipToken token
  moves  = Board.possibleMoves board
  states = map (Board.playMove board token) moves
  scores = map (\board' -> score board' token' False 0) states
