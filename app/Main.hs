
module Main where

    import Data.Strings (strSplit)
    import Data.Either
    import Text.Read (readMaybe)
    import System.IO
    import Data.List (find)
    import Critic (SEV)
    import Actor (actorControl, ActorState, State)
    
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
        loopAskSquareMsg esp
    loopAskSquareMsg (Right p) =
        return p
    
    playerSquareMsg :: Player -> String
    playerSquareMsg p = show p ++ " pick a row column index between 0 an 2"
    
    loopMove :: Board -> Square -> Player -> IO (Board, Player)
    loopMove b sq p =
        if p /= np then
            return (nb, np)
        else do
            putStrLn "Choose a different square."
            esp <- readSquare (playerSquareMsg p)
            nsq <- loopAskSquareMsg esp
            loopMove b nsq p
        where (nb, np) = move b sq p
    
    putSpace :: IO()
    putSpace = putChar ' '
    
    printSquare :: XO -> IO()
    printSquare E = putSpace
    printSquare X = putChar 'x'
    printSquare O = putChar 'o'
        
    printSpacedSquare :: XO -> IO()
    printSpacedSquare s = do
        putSpace
        printSquare s
        putSpace
    
    printRow :: Row -> IO()
    printRow [s0, s1, s2] = do
        printSpacedSquare s0
        putChar '|'
        printSpacedSquare s1
        putChar '|'
        printSpacedSquare s2
    
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
            esp <- readSquare (playerSquareMsg p)
            sq <- loopAskSquareMsg esp
            (nb, np) <- loopMove b sq p
            printBoard nb
            play nb np

    --main = mapM printBoard (aiNonTerminalStates P2)
    --main = print(length(aiNonTerminalStates P1))
    --main = print (possibleMoves [[X,X,O],[E,E,E],[E,E,E]])
    main = 