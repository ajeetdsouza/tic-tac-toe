module Player.Human
  ( getMove
  )
where

import           Data.Char                      ( ord
                                                , toLower
                                                )
import           Data.Maybe                     ( isNothing )
import           System.IO                      ( hFlush
                                                , stdout
                                                )

import           Board
import           Move
import           Pretty
import           Token

getMove :: Board -> Token -> IO Move
getMove (Board board) token = do
  putStr $ "Enter your move (" ++ pretty token ++ "): "
  hFlush stdout
  input <- getLine
  if length input < 2 then retry else toMove input
 where
  retry = do
    putStrLn "Invalid input, please try again."
    getMove (Board board) token

  toMove (x : y : _) =
    let x' = (ord . toLower) x - ord 'a'
        y' = ord y - ord '1'
    in  if 0 <= x' && x' <= 2 && 0 <= y' && y' <= 2 && isNothing
           (board !! x' !! y')
        then
          return $ Move (x', y')
        else
          retry
