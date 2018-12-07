module TicTacToe where
    
    data Move = Move Int Int deriving (Show, Eq)
    data XO = E | X | O deriving (Eq)
    data Player = P1 | P2 deriving (Eq)
    type Row = [XO]
    type Board = [Row]
    type Moves = [Move]
    type States = [Board]
    
    instance Show XO where
        show X = "x"
        show O = "o"
        show E = " "

    instance Show Player where
        show P1 = "Player 1"
        show P2 = "Player 2"

    randomBoard :: Board
    randomBoard = [[X,E,O],[E,E,E],[E,E,E]]

    initRow :: Row
    initRow = [E, E, E]
    
    initBoard :: Board
    initBoard = [initRow, initRow, initRow]

    nextPlayer :: Player -> Player
    nextPlayer P1 = P2
    nextPlayer P2 = P1

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
    
    move :: Board -> Player -> Move -> (Board, Player)
    move [r0, r1, r2] p (Move 0 y)= 
        ([nr, r1, r2], np)
        where (nr, np) = setSquare r0 y p                            
    move [r0, r1, r2] p (Move 1 y) =
        ([r0, nr, r2], np)
        where (nr, np) = setSquare r1 y p
    move [r0, r1, r2] p (Move 2 y)=
        ([r0, r1, nr], np)
        where (nr, np) = setSquare r2 y p
    move b p sq= (b, p)

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

    notFinished :: Board -> Bool
    notFinished [r0 r1 r2] = any (== E) r0 || (any (== E) r1) || (any (== E) r2)

    isDraw :: Board -> Bool
    isDraw b = x == 5 && o == 4
        where (x, o) = countDoneMoves b

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

    playerStates :: Player -> States
    playerStates ai = filter (\b -> notWin b P1 && notWin b P2 && (isPlayerMove b ai)) allStates

    allRowMoves :: Moves -> (Int, Row) -> Moves
    allRowMoves ss (i, r) = (foldl (\ss' -> \t -> if snd t == E then (Move i (fst t)):ss' else ss') ss (zip [0..2] r))

    allMoves :: Board -> Moves
    allMoves b = foldl allRowMoves [] (zip [0..2] b)

    allNextBoardsByMove :: Player -> Board -> [(Move, Board)]
    allNextBoardsByMove p b = map (\m -> (m, fst (move b p m)) (allMoves b)

    allNextBoards :: Player -> Board -> [Board]
    allNextBoards p b = map (fst . (move b p)) (allMoves b)

    allReactionsByMove :: Player -> Board -> [(Move, [Board])]
    allReactionsByMove p b = foldl (\bs -> \t -> (fst t, allNextBoards (nextPlayer p) (snd t))++bs) [] (allNextBoardsByMove p b)