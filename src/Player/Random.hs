module Player.Random
  ( getMove
  )
where

import           System.Random                  ( randomRIO )

import           Board
import           Move
import           Token

getMove :: Board -> Token -> IO Move
getMove (Board board) token = do
  let moves = validMoves (Board board)
  idx <- randomRIO (0, length moves - 1)
  return $ moves !! idx
