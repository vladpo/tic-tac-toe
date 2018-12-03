module Main where

    import Data.Strings (strSplit)
    import Data.Either
    import Text.Read (readMaybe)
    import System.IO
    import Data.List (find)
    
    data Square = Square Int Int deriving (Show, Eq)
    data XO = E | X | O deriving (Eq)
    data Player = P1 | P2 deriving (Eq)
    type Row = [XO]
    type Board = [Row]
    type Moves = [Square]
    type States = [Board]
    
    instance Show XO where
        show X = "x"
        show O = "o"
        show E = " "

    instance Show Player where
        show P1 = "Player 1"
        show P2 = "Player 2"

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
    
    initRow :: Row
    initRow = [E, E, E]
    
    initBoard :: Board
    initBoard = [initRow, initRow, initRow]

    nextPlayer :: Player -> Player
    nextPlayer P1 = P2
    nextPlayer P2 = P1
    
    playerSquareMsg :: Player -> String
    playerSquareMsg p = show p ++ " pick a row column index between 0 an 2"
    
    sign :: Player -> XO
    sign P1 = X
    sign P2 = O

    setSquare :: Row -> Int -> Player -> (Row, Player)
    setSquare [s0, s1, s2] 0 p =
        if s0 /= E then ([s0, s1, s2], p) else ([sign(p), s1, s2], nextPlayer(p))
    setSquare [s0, s1, s2] 1 p =
        if s1 /= E then ([s0, s1, s2], p) else ([s0, sign(p), s2], nextPlayer(p))
    setSquare [s0, s1, s2] 2 p =
        if s2 /= E then ([s0, s1, s2], p) else ([s0, s1, sign(p)], nextPlayer(p))
    setSquare r _ p = (r, p)
    
    move :: Board -> Square -> Player -> (Board, Player)
    move [r0, r1, r2] (Square 0 y) p = 
        ([nr, r1, r2], np)
        where (nr, np) = setSquare r0 y p                            
    move [r0, r1, r2] (Square 1 y) p =
        ([r0, nr, r2], np)
        where (nr, np) = setSquare r1 y p
    move [r0, r1, r2] (Square 2 y) p =
        ([r0, r1, nr], np)
        where (nr, np) = setSquare r2 y p
    move b sq p = (b, p)
    
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
    
    isRowWin :: Board -> Player -> Bool
    isRowWin b p = any (\r -> r!!0 == sign(p) && r!!0 == r!!1 && r!!1 == r!!2) b
    
    isColWin :: Board -> Player -> Bool
    isColWin [r0, r1, r2] p = any (\col -> r0!!col == sign(p) && r0!!col == r1!!col && r1!!col == r2!!col) [0..2]

    isDigWin :: Board -> Player -> Bool
    isDigWin [r0, r1, r2] p = (r0!!0 == sign(p) && r0!!0 == r1!!1 && r1!!1== r2!!2) || (r0!!2 /= E && r0!!2 == r1!!1 && r1!!1 == r2!!0)

    isWin :: Board -> Player -> Bool
    isWin b p = isRowWin b p || isColWin b p || isDigWin b p

    notWin :: Board -> Player -> Bool
    notWin b p = not(isWin b p)

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

    randomBoard :: Board
    randomBoard = [[X,E,O],[E,E,E],[E,E,E]]

    main = print(findValue randomBoard (initSvf P1))

    incMove :: (Int, Int) -> XO -> (Int, Int)
    incMove (x, o) xo = 
        if xo == X then (x + 1, o)
        else if xo == O then (x, o + 1)
        else (x, o)

    countRowMoves :: Row -> (Int, Int)
    countRowMoves r = foldl incMove (0, 0) r

    incRowMoves :: (Int, Int) -> Row -> (Int, Int)
    incRowMoves (x, o) r =
            (x + x', o + o')
        where (x', o') = countRowMoves r

    countDoneMoves :: Board -> (Int, Int)
    countDoneMoves b = foldl (incRowMoves) (0, 0) b

    isPlayerMove :: Board -> Player -> Bool
    isPlayerMove b p = 
        if sign p == X then x == o
        else x == (o + 1)
        where (x, o) = countDoneMoves b

    rows :: [[XO]]
    rows = [s| s <- mapM (const [O, X, E]) [0..2]]

    allStates :: States
    allStates = [b| b <- mapM (const rows) [0..2]]

    aiNonTerminalStates :: Player -> States
    aiNonTerminalStates ai = filter (\b -> notWin b P1 && notWin b P2 && (isPlayerMove b ai)) allStates

    possibleRowMoves :: Moves -> (Int, Row) -> Moves
    possibleRowMoves ss (i, r) = (foldl (\ss' -> \t -> if snd t == E then (Square i (fst t)):ss' else ss') ss (zip [0..2] r))

    possibleMoves :: Board -> Moves
    possibleMoves b = foldl possibleRowMoves [] (zip [0..2] b)

    type StateValue = (Board, Float)
    type EligibilityTrace = [StateValue]
    type SVF = [StateValue]
    type State = Board
    type Reward = Float

    initStateValues :: Player -> [StateValue]
    initStateValues p = zip ss (replicate (length ss) 0)
        where ss = aiNonTerminalStates p

    findValue :: State -> SVF -> Maybe Float
    findValue b svf = fmap snd (find (\t -> fst t == b) svf)

    eligibility :: EligibilityTrace -> State -> Float -> Maybe Float
    eligibility et s a = findValue s et 

    evaluate :: SVF -> SVF -> State -> State -> Reward -> SVF
    evaluate oldSvf svf oldS s r  = 
        where 
            d = findValue oldS svf - findValue oldS oldSvf
            err = r + g + (findValue s svf - findValue oldS svf)

