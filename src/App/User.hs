module App.User where

import           Data.Char                      ( toLower
                                                , ord
                                                )
import           Data.Maybe                     ( isNothing
                                                , fromJust
                                                )
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import           Text.Read                      ( readMaybe )
import qualified App.Board                     as Board

invalidMsg :: String
invalidMsg = "Invalid input. Please try again.\n"

readMove :: Board.Board -> Char -> IO Board.Move
readMove board token = do
  putStr $ "Your move (" ++ [token] ++ "): "
  hFlush stdout
  input <- getLine

  if length input >= 2
    then
      let x = toLower . head $ input
          y = readMaybe [input !! 1] :: Maybe Int
      in  if not ('a' <= x && x <= 'c') || isNothing y
            then retry
            else
              let x' = fromJust y - 1
                  y' = ord x - ord 'a'
              in  if board !! x' !! y' == '-' then return (x', y') else retry
    else retry
 where
  retry = do
    putStrLn invalidMsg
    readMove board token


readToken :: IO Char
readToken = do
  putStr "Pick (x/o): "
  hFlush stdout
  playerToken <- head <$> getLine

  if playerToken `elem` "xo"
    then return playerToken
    else do
      putStrLn invalidMsg
      readToken
