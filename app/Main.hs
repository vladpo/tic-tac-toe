
module Main where

import Data.Strings (strSplit)
import Data.Either
import Text.Read (readMaybe)
import System.IO
import Data.List (find)
import TicTacToe ( Move
                  , Move (M)
                  , Player
                  , Player (P1)
                  , Player (P2)
                  , Board
                  , XO
                  , XO (O)
                  , XO (X)
                  , XO (E)
                  , Row
                  , nextPlayer
                  , isWin
                  , move
                  , initBoard
                  , notFinished
                  , isFinished
                  , isDraw
                  )
import ActorCritic
import qualified System.Random                   as Rand (mkStdGen)
import qualified Control.Monad.State.Lazy        as ST ( execState
                                                      , evalState
                                                      )
import qualified Control.Monad.Reader            as R (runReaderT)

strMove :: (String, String) -> Either String Move
strMove (x, y) =
  case readMaybe x >>= (\x' -> fmap (\y' -> M x' y') (readMaybe y)) of
      Just p -> Right p
      Nothing -> Left "You need to provide valid row/column indexes."

parseMove :: String -> Either String Move
parseMove s = strMove (strSplit " " s)

validate :: Either String Move -> Either String Move
validate (Right (M x y)) =
  if x >= 0 && x <= 2 && y >= 0 && y <= 2 then
      Right (M x y)
  else
      Left "Your row/column values should be between 0 and 2."
validate e = e

readMove :: String -> IO (Either String Move)
readMove msg = do
  putStrLn(msg)
  p <- getLine
  return (validate (parseMove p))

loopAskMoveMsg :: Either String Move -> IO Move
loopAskMoveMsg (Left e) = do
  esp <- readMove (e ++ " Let's try again:")
  loopAskMoveMsg esp
loopAskMoveMsg (Right p) =
  return p

playerMoveMsg :: Player -> String
playerMoveMsg p = show p ++ " pick a row column index between 0 an 2"

loopMove :: Board -> Move -> Player -> IO (Board, Player)
loopMove b m p =
  if p /= np then
      return (nb, np)
  else do
      putStrLn "Choose a different square."
      esp <- readMove (playerMoveMsg p)
      nsq <- loopAskMoveMsg esp
      loopMove b nsq p
  where (nb, np) = move b p m

putSpace :: IO()
putSpace = putChar ' '

printMove :: XO -> IO()
printMove E = putSpace
printMove X = putChar 'x'
printMove O = putChar 'o'
  
printSpacedMove :: XO -> IO()
printSpacedMove s = do
  putSpace
  printMove s
  putSpace

printRow :: Row -> IO()
printRow [s0, s1, s2] = do
  printSpacedMove s0
  putChar '|'
  printSpacedMove s1
  putChar '|'
  printSpacedMove s2

printRowSep :: IO()
printRowSep = do
  putStrLn ""
  putStrLn "___________"

printBoard :: Board -> IO()
printBoard [r0, r1, r2] = do
  printRow r0
  printRowSep
  printRow r1
  printRowSep
  printRow r2
  putStrLn ""
  putStrLn ""

play :: Board -> Player -> IO()
play b p = 
  if isWin b (nextPlayer p) then
      print(show (nextPlayer p) ++ " won the game. Congrats.")
  else do
      esp <- readMove (playerMoveMsg p)
      m <- loopAskMoveMsg esp
      (nb, np) <- loopMove b m p
      printBoard nb
      play nb np

playAI ::
  Board ->
  Player ->
  [ActorState] ->
  IO ()
playAI b p ass = do
  if isFinished b && isDraw b then do
    print("Draw.")
    playAI initBoard P1 ass
  else if isWin b (nextPlayer p) then do
    print(show (nextPlayer p) ++ " won the game. Congrats.")
    playAI initBoard P1 ass
  else
    let 
      mio = 
        if p == P1 then
          return . _aAction . maximum $ (flip filter ass $ (==b) . _aState)
        else do
          esp <- readMove (playerMoveMsg p)
          loopAskMoveMsg esp
    in do
      m <- mio
      (nb, np) <- loopMove b m p
      printBoard nb
      playAI nb np ass

--main = mapM printBoard (aiNonTerminalStates P2)
--main = print(length(aiNonTerminalStates P1))
--main = print (possibleMoves [[X,X,O],[E,E,E],[E,E,E]])
main :: IO ()
main = playAI initBoard P1 ass
  where 
    ass:: [ActorState]
    ass = (fst . fst . fst . snd) (learn (config (Rand.mkStdGen 13) 0.4 0.9 0.5)  (Rand.mkStdGen 13, (((initASs P1, 0.0), (initCSs P1, 0.0)), ((initASs P2, 0.0), (initCSs P2, 0.0)))) 0 P1 initBoard)
--main = print (((initASs P1, 0.0), (initCSs P1, 0.0)), ((initASs P2, 0.0), (initCSs P2, 0.0)))
--main = print (notFinished [[X,X,O],[O,O,X],[O,X,E]])
-- main = print (notFinished initBoard)
-- main = print (initCSs P2)
--main = print (initASs P1)