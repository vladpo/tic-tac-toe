module Main where

import Data.Strings (strSplit)
import Data.Either
import Text.Read (readMaybe)
import System.IO

data Square = Square Int Int deriving (Show, Eq)
data XO = EMPTY | X | O deriving (Show, Eq)
data Player XO = P1 X | P2 O deriving (Show, Eq)
type Board = [Row, Row, Row]
type Row = [XO, XO, XO]

strSquare :: (String, String) -> Either String Square
strSquare (x, y) =
    case readMaybe x >>= (\x' -> fmap (\y' -> Square x' y') (readMaybe y)) of
        Just p -> Right p
        Nothing -> Left "You need to provide valid row/column indexes."

parseSquare :: String -> Either String Square
parseSquare s = strSquare (strSplit " " s)

validate :: Either String Square -> Either String Square
validate (Right (Square x y)) =
    if x >= 0 && x <= 2 && y >= 0 && y <= 2 then
        Right (Square x y)
    else
        Left "Your row/column values should be between 0 and 2."
validate e = e

readSquare :: String -> IO (Either String Square)
readSquare msg = do
    putStrLn(msg)
    p <- getLine
    return (validate (parseSquare p))

loopAskSquareMsg :: Either String Square -> IO Square
loopAskSquareMsg (Left e) = do
    esp <- readSquare (e ++ " Let's try again:")
    loopAskPosMsg esp
loopAskSquareMsg (Right p) =
    return p

initRow :: Row
initRow = [EMPTY, EMPTY, EMPTY]

initBoard :: Board
initBoard = [initRow, initRow, initRow]

nextPlayer :: Player -> Player
nextPlayer P1 _ = P2 O
nextPlayer P2 _ = P1 X

moveCol :: Row -> Int -> Player -> (Row, Player)
moveCol [s0, s1, s2] 0 (Player xo) =
    if s0 <> EMPTY then ([s0, s1, s2], Player xo) else ([xo, s2, s3], nextPlayer(Player xo))
moveCol [s0, s1, s2] 1 (Player xo) =
    if s1 <> EMPTY then ([s0, s1, s2], Player xo) else ([s1, xo, s3], nextPlayer(Player xo))
moveCol [s0, s1, s2] 2 (Player xo)=
    if s2 <> EMPTY then ([s0, s1, s2], Player xo) else ([s0, s1, xo], nextPlayer(Player xo))
moveCol r = (r, Player xo)

move :: Board -> Square -> Player -> (Board, Player)
move [r0, r1, r2] (Square 0 y) p =
    let (b, np) = moveCol r0 y p
        ([moveCol r0 y p, r1, r2], np)
move [r1, r2, r3] (Square 1 y) p =
    let (b, np) = moveCol r1 y p
        ([r0, moveCol r1 y p, r2], np)
move [r1, r2, r3] (Square 2 y) p =
    let (b, np) = moveCol r2 y p
        ([r0, r1, moveCol r2 y p], np)
move b p = (b, p)

loopMove :: Board -> Square -> Player -> Board
loopMove b sq p =
    let (nb, np) = move b sq p

play :: Booar -> u
main = do
    esp <- readSquare "Player 1 pick a row column index between 0 an 2"
    sq <- loopAskSquareMsg esp
    move initBoard sq (P1 X)