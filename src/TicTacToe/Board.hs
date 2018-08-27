module TicTacToe.Board where

import           Data.Either                    ( fromRight )
import           Data.List                      ( elemIndices
                                                , transpose
                                                )

import           TicTacToe.Move                as Move
import           TicTacToe.Token               as Token

newtype Board = Board [String]

instance Show Board where
    show (Board board) = unlines . map (unwords . map toList) $ board'
     where
      toList   = (: [])

      upHeader = ['•', '1', '2', '3']
      ltHeader = ['a', 'b', 'c']

      board'   = upHeader : zipWith (:) ltHeader board

new :: Board
new = Board . replicate 3 . replicate 3 $ '-'

playMove :: Board -> Token.Token -> Move.Move -> Either String Board
playMove (Board board) token (Move x y)
  | board !! x !! y == '-' = Right . Board $ board'
  | otherwise              = Left "Invalid move"
 where
  (rows1, row : rows2) = splitAt x board
  (cols1, _ : cols2  ) = splitAt y row

  row'                 = cols1 ++ show token ++ cols2
  board'               = rows1 ++ [row'] ++ rows2

isFull :: Board -> Bool
isFull (Board board) = not . any (elem '-') $ board

isWinner :: Board -> Token.Token -> Bool
isWinner (Board board) token =
  winSequence
    `elem` board
    ||     winSequence
    `elem` transpose board
    ||     zipWith (!!) board [0, 1, 2]
    ==     winSequence
    ||     zipWith (!!) board [2, 1, 0]
    ==     winSequence
  where winSequence = take 3 . cycle $ show token

validMoves :: Board -> [Move.Move]
validMoves (Board board) = map
  (fromRight (error "Invalid move generated") . uncurry Move.mkMove)
  moveTuples
 where
  moveTuples =
    concatMap (\(idx, ys) -> zip (repeat idx) ys) . zip [0 ..] $ moveYs
  moveYs = map (elemIndices '-') board
