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
                   , eligibility :: Double
                   , value :: Double
                   } deriving (Show)
    type Reward = Double

    γ :: Double
    γ = 0.9

    α :: Double
    α = 0.9

    λ :: Double
    λ = 0.9

    initSev :: State -> SEV
    initSev s = SEV {state=s, eligibility=0.0, value=0.0}

    initSevs :: Player -> [SEV]
    initSevs p = map initSev (aiNonTerminalStates p)

    nowNextSev :: [SEV] -> State -> State -> (SEV, SEV)
    nowNextSev sevs s s'= foldl(\nowNext -> \sev -> 
                                                    if (state sev == s) then (sev, snd nowNext)
                                                    else if (state sev == s') then (fst nowNext, sev)
                                                    else nowNext
                               ) (initSev s, initSev s') sevs
        
    -- True online TD(λ) with eligibility dutch-traces
    criticEvaluate :: [SEV] -> State -> State -> Reward -> Double -> Maybe ([SEV], Double)
    criticEvaluate sevs s s' r oldv = (map update sevs, err)
        where 
            nowNext = nowNextSev sevs s s'
            err = r + γ*(value . snd nowNext) - (value . fst nowNext)
            Δ = value . fst nowNext - oldv
            oldv' = value . snd nowNext
            e = (1-α)*(eligibility . fst nowNext) + 1
            update = \sev ->
                            if state sev == s then
                                SEV {state=s, eligibility=(γ*λ*e), value=(value sev + α*(err + Δ)*e - α*Δ)}
                            else
                                SEV {state=(state sev), eligibility=(γ*λ*(eligibility sev)), value=(value sev + α*(err + Δ)*(eligibility sev))}

    
    type Action = Square
    data ActorState = ActorState { state :: State
                       , action :: Action
                       , probability :: Double
                       , preference :: Double
                       , eligibility :: Double
                       , value :: Double
                       }
    data Result = Result { now :: ActorState
                         , next :: ActorState
                         , sum :: Double
                         }
    
    initAS :: ActorState
    initAS s a = ActorState {state=s, action=a, preference=0.0, eligibility=0.0, value=0.0}

    initASs :: Player -> [ActorState]
    initASs p = map (\s -> map (\a -> initAS s a) (possibleMoves s)) (aiNonTerminalStates p)

    nowNextActorState :: [ActorState] -> ((State, Action), (State, Action)) -> Result
    nowNextActorState ass ((s,a), (s',a'))= 
        foldl (\result -> \as -> 
                                if (state as == s && (action as == a)) then Result{now=as, next=(next result), sum=(sum result)}
                                else if (state as == s' && (action as == a')) then Result{now=(now result), next=as, sum=(sum result + (preference as))}
                                else Result{now=(now result), next=(next result), sum=(sum result + 2.71828**(preference as))}
              ) (Result {now=(initAS s a), next=(initAS s' a'), sum=0.0}) ass

    -- True online Sarsa(λ), with eligibility traces and with a policy using a gradient ascent distribution (Gibbs distribution)
    actorControl :: [ActorState] -> (State, Action) -> Reward -> Double -> Double -> [ActorState]
    actorControl ass (s,a) oldV cErr = 
        where
            result = nowNextActorState ass ((s,a), (s',a'))
            err = r + γ*(value . next result) - (value . now result))
            Δ = value . now result - oldv
            decayEligibility = \gl -> gl*(eligibility . now result) + 1 - (probability . now result))
            E = decayEligibility 1.0
            γλE = decayEligibility (γ*λ)
            oldv' = value . next result
            update = \as ->
                            if state as == s then do
                                ActorState { state=s
                                           , action=a
                                           , probability=(2.71828**(preference as + α*cErr*E)/(sum result))
                                           , preference=(preference as + α*cErr*E)
                                           , eligibility=γλE
                                           , value=(value as + α*(err + Δ)*E - α*Δ)
                                           }
                            else
                                ActorState { state=(state as)
                                           , action=(action as)
                                           , probability=???
                                           , preference=(preference as)
                                           , eligibility=(γ*λ*(eligibility as))
                                           , value=(value as + α*(err + Δ)*(eligibility as))
                                           }