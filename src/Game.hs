module Game
  ( Game.new
  )
where

import           Control.Monad                  ( forM_ )
import           Data.Maybe                     ( fromMaybe )
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import           Text.Read                      ( readMaybe )
import           Board
import           Move
import           Player
import           Token

new :: [Player] -> IO ()
new playerOptions = do
  player1 <- getPlayer X playerOptions
  player2 <- getPlayer O playerOptions

  let board = Board.new
  print board

  winner <- playTurn X board player1 player2
  putStrLn $ case winner of
    Nothing    -> "It's a draw!"
    Just token -> "Player " ++ show token ++ " wins!"

playTurn :: Token -> Board -> Player -> Player -> IO (Maybe Token)
playTurn token board player1 player2 = do
  move <- getMove player1 board token
  putStrLn $ "Player " ++ show token ++ " move: " ++ show move

  let board' = fromMaybe (error "getMove returned an illegal move!")
                         (Board.playMove board token move)
  print board'

  if Board.isWinner board' token
    then return $ Just token
    else if Board.isFull board'
      then return Nothing
      else playTurn (flipToken token) board' player2 player1

getPlayer :: Token -> [Player] -> IO Player
getPlayer token playerOptions = do
  putStrLn $ "Player " ++ show token ++ ": "
  forM_ (zip [1 ..] playerOptions)
    $ \(idx, player) -> putStrLn $ show idx ++ ". " ++ show player

  putStr ">> "
  hFlush stdout

  input <- getLine
  putStrLn ""

  case parseInput input of
    Nothing     -> retry
    Just player -> return player
 where
  parseInput input = do
    n <- flip (-) 1 <$> readMaybe input
    if 0 <= n && n < length playerOptions
      then return $ playerOptions !! n
      else Nothing

  retry = do
    putStrLn "Invalid input, please try again."
    getPlayer token playerOptions
