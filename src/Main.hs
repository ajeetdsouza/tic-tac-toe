module Main where

import           System.IO                      ( hFlush
                                                , stdout
                                                )
import           Text.Read                      ( readMaybe )

import qualified App.Board                     as Board
import qualified App.Game                      as Game
import qualified App.User                      as User

main :: IO ()
main = do
  putStr
    . unlines
    $ [ "Pick an option: "
      , "1. Human vs. Human"
      , "2. Human vs. AI"
      , "3. AI vs AI"
      ]
  putStr ">> "
  hFlush stdout

  option <- readMaybe <$> getLine :: IO (Maybe Int)
  case option of
    Nothing -> retry
    Just 1  -> Game.play board 'x' Game.Human Game.Human
    Just 2  -> do
      putStr "Pick (x/o): "
      hFlush stdout
      input <- getLine
      case head input of
        'x' -> Game.play board 'x' Game.Human Game.AI
        'o' -> Game.play board 'x' Game.AI Game.Human
        _   -> retry
    Just 3 -> Game.play board 'x' Game.AI Game.AI
    Just _ -> retry
 where
  board = Board.empty
  retry = do
    putStrLn User.invalidMsg
    main

