module Board where

import           Data.List                      ( elemIndices
                                                , transpose
                                                )
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                )

import           Move
import           Token

newtype Board = Board [[Maybe Token]]

instance Show Board where
  show (Board board) = unlines . map unwords $ board'
   where
    showToken (Just token) = show token
    showToken Nothing = "-"

    rowHead = ["a", "b", "c"]
    colHead = ["•", "1", "2", "3"]

    board' = colHead : zipWith (:) rowHead (map (map showToken) board)

new :: Board
new = Board . replicate 3 . replicate 3 $ Nothing

validMoves :: Board -> [Move]
validMoves (Board board) = concatMap (\(x, ys) -> map (\y -> Move (x, y)) ys)
                                     (zip [0 ..] cols)
  where cols = map (elemIndices Nothing) board

playMove :: Board -> Token -> Move -> Maybe Board
playMove (Board board) token (Move (x, y)) = if isNothing (board !! x !! y)
  then Just $ Board board'
  else Nothing
 where
  (rows1, row : rows2) = splitAt x board
  (cols1, _ : cols2  ) = splitAt y row

  row'                 = cols1 ++ [Just token] ++ cols2
  board'               = rows1 ++ [row'] ++ rows2

isFull :: Board -> Bool
isFull (Board board) = all (all isJust) board

isWinner :: Board -> Token -> Bool
isWinner (Board board) token =
  checkRows board || checkCols board || checkDiag1 board || checkDiag2 board
 where
  checkRows = any (all (== Just token))
  checkCols = checkRows . transpose

  checkDiag1 board = all (== Just token) $ zipWith (!!) board [0, 1, 2]
  checkDiag2 board = all (== Just token) $ zipWith (!!) board [2, 1, 0]
