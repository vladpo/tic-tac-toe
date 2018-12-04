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

    type State = Board
    data SEV = SEV { state :: State
                   , eligibility :: Float
                   , value :: Float
                   } deriving (Show)
    type Reward = Float

    γ :: Float
    γ = 0.9

    α :: Float
    α = 0.9

    λ :: Float
    λ = 0.9

    initSevs :: Player -> [SEV]
    initSevs p = map (\s -> SEV {state=s, eligibility=0.0, value=0.0}) (aiNonTerminalStates p)

    findSev :: [SEV] -> (State, State) -> Maybe (SEV, SEV)
    findSev sevs (s, s')= (sevs'!!0, sevs'!!1)
        where sevs' = filter (\sev -> state sev == s || state sev == s') sevs
        
    -- True online TD(λ) with eligibility dutch-traces
    criticEvaluate :: [SEV] -> State -> State -> Reward -> Float -> Maybe ([SEV], Float)
    criticEvaluate sevs s s' r oldv = fmap (\err -> (map update sevs, err) merr
        where 
            mt = findSev sevs (s, s')
            merr = fmap (\t -> r + γ*(value . snd t) - (value . fst t)) mt
            mΔ = fmap (\t -> value . fst t - oldv) mt
            oldv' = fmap (\t -> value . snd t) mt
            me = fmap (\t -> (1-α)*(eligibility . fst t) + 1) mt
            maybeUpdate sev =
                if state sev == s then do
                    err <- merr
                    Δ <- mΔ
                    e <- me
                    return (s, γ*λ*e, value sev + α*(err + Δ)*e - α*Δ)
                else
                    Just (state sev, γ*λ*(eligibility sev), value sev + α*(err + Δ)*(eligibility sev))
            update sev = case maybeUpdate sev of
                Just sev' = sev'
                Nothing = sev

    
    type Action = Square
    data ActorState = ActorState { state :: State
                       , action :: Action
                       , probability :: Float
                       , preference :: Float
                       , eligibility :: Float
                       , value :: Float
                       }
    
    initActorStates :: Player -> [ActorState]
    initActorStates p = map (\s -> map (\a -> ActorState {state=s, action=a, preference=0.0, eligibility=0.0, value=0.0}) (possibleMoves s)) (aiNonTerminalStates p)

    findActorStates :: [ActorState] -> ((State, Action), (State, Action)) -> Maybe (ActorState, ActorState)
    findActorStates ass ((s,a), (s',a'))= (ass'!!0, ass'!!1)
        where ass' = filter (\as -> (state as == s && (action as == a)) || (state as == s' && (action as == a'))) ass

    -- True online Sarsa(λ), with eligibility traces and gradient ascent policy distribution (Gibbs distribution)
    actorControl :: [ActorState] -> (State, Action) -> (State, Action) -> Reward -> Float -> Float -> [ActorState]
    actorControl ass s oldV cErr = 
        where
            mt = findActorStates ass ((s,a), (s',a'))
            merr = fmap (\t -> r + γ*(value . snd t) - (value . fst t)) mt
            mΔ = fmap (\t -> value . fst t - oldv) mt
            me = fmap (\t -> (eligibility . fst t) + 1 - (probability . fst t)) mt
            maybeUpdate as =
                if state as == s then do
                    err <- merr
                    Δ <- mΔ
                    e <- me
                    return (s, a, ???, preference as + λ*cErr*(1 - probability as), γ*λ*e, value as + α*(err + Δ)*e - α*Δ)
                else
                    Just (state as, action as, ???, preference as, γ*λ*(eligibility as), value as + α*(err + Δ)*(eligibility as))
            update as = case maybeUpdate as of
                Just as' = as'
                Nothing = as