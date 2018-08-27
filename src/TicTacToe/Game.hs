module TicTacToe.Game where

import           Data.Either                    ( fromRight )
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import           Text.Read                      ( readMaybe )

import           TicTacToe.AI                  as AI
import           TicTacToe.Board               as Board
import           TicTacToe.Move                as Move
import           TicTacToe.Token               as Token

data Player = AI | Human deriving (Eq, Show)

msgInvalid :: String
msgInvalid = "Invalid input, please try again."

msgPrompt :: String
msgPrompt = ">> "

new :: IO ()
new = do
  putStrLn $ "Player " ++ show Token.X ++ ": "
  player1 <- getPlayer
  putStrLn ""

  putStrLn $ "Player " ++ show Token.O ++ ": "
  player2 <- getPlayer
  putStrLn ""

  let board = Board.new

  turn board player1 player2 Token.X
 where
  getPlayer = do
    putStr . unlines $ ["1. " ++ show AI, "2. " ++ show Human]
    putStr msgPrompt
    hFlush stdout

    input <- getLine
    case readMaybe input :: Maybe Int of
      Just 1 -> return AI
      Just 2 -> return Human
      _      -> do
        putStrLn msgInvalid
        getPlayer

aiPlayMove :: Board.Board -> Token.Token -> IO Board.Board
aiPlayMove board token = do
  putStrLn $ "Player " ++ show token ++ " move: " ++ show move
  return
    . fromRight (error "Invalid move generated")
    . Board.playMove board token
    $ move
  where move = AI.getMove board token

humanPlayMove :: Board.Board -> Token.Token -> IO Board.Board
humanPlayMove board token = do
  putStr $ "Player " ++ show token ++ " move: "
  hFlush stdout

  move <- readMaybe <$> getLine :: IO (Maybe Move.Move)
  case move of
    Nothing    -> retry
    Just move' -> case Board.playMove board token move' of
      Left  _      -> retry
      Right board' -> return board'
 where
  retry = do
    putStrLn msgInvalid
    humanPlayMove board token

turn :: Board.Board -> Player -> Player -> Token.Token -> IO ()
turn board player1 player2 token = do
  print board
  turn'
 where
  token' = Token.other token
  turn'
    | Board.isWinner board token' = putStrLn $ show token' ++ " wins!"
    | Board.isFull board = putStrLn "It's a draw!"
    | otherwise = do
      board' <- case player1 of
        Human -> humanPlayMove board token
        AI    -> aiPlayMove board token
      turn board' player2 player1 token'
