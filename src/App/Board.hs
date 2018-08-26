{-# LANGUAGE TupleSections #-}

module App.Board where

import           Data.List                      ( elemIndices
                                                , transpose
                                                )

type Board = [String]
type Move = (Int, Int)

empty :: Board
empty = replicate 3 "---"

win :: Board -> Char -> Bool
win board token =
  winSequence
    `elem` board
    ||     winSequence
    `elem` transpose board
    ||     zipWith (!!) board [0, 1, 2]
    ==     winSequence
    ||     zipWith (!!) board [2, 1, 0]
    ==     winSequence
  where winSequence = replicate 3 token

move :: Board -> Char -> Move -> Board
move board token (x, y) = board'
 where
  (rows1, row : rows2) = splitAt x board
  (cols1, _ : cols2  ) = splitAt y row

  row'                 = cols1 ++ [token] ++ cols2
  board'               = rows1 ++ [row'] ++ rows2

display :: Board -> IO ()
display board = putStrLn . unlines . map (unwords . map (: [])) $ board'
 where
  upHeader = ['•', '1', '2', '3']
  ltHeader = ['a', 'b', 'c']

  board'   = upHeader : zipWith (:) ltHeader board

movesLeft :: Board -> Int
movesLeft = sum . map (length . elemIndices '-')

flipToken :: Char -> Char
flipToken token = if token == 'x' then 'o' else 'x'

possibleMoves :: Board -> [Move]
possibleMoves =
  concat . zipWith (\idx -> map (idx, )) [0 ..] . map (elemIndices '-')
