{-# LANGUAGE TemplateHaskell #-}
module Actor where

    import TicTacToe (playerStates, allReactionsByMove, allMoves, move, isWin, notFinished, isDraw)
    import Critic (criticEvaluate, SEV)
    import Control.Lens
    import Numeric.Probability.Random as NPRandom
    import Numeric.Probability.Distribution as Dist
    import qualified System.Random as Random
    import System.Random (Random, )

    type Action = Square
    type Err = Double
    data ActorState = ActorState { _state :: State
                       , _action :: Action
                       , _probability :: Double
                       , _preference :: Double
                       , _eligibility :: Double
                       , _value :: Double
                       } deriving (Show)
    makeLenses ''ActorState

    instance Eq ActorState where
         x == y = _state x == _state y && _action x == _action y

    type Actor = ([ActorState], Double)

    actorEvaluate :: (Board, Board) -> Double -> Reward -> State Actor ()
    actorEvaluate (s, s') r
        = do (ass, oldV) <- get
             put (update <$> ass, oldV')
                where
                    err = r + γ*(_value s') - (_value s)
                    Δ = _value s - oldV
                    decayEligibility = \γλ -> γλ*(_eligibility s) + 1.0 - (_probability s)
                    E = decayEligibility 1.0
                    γλE = decayEligibility (γ*λ)
                    oldV' = _value . s'
                    update :: ActorState -> ActorState
                    update as =
                        if (as == s) then (over preference (+ α*cErr*E)) . (set eligibility γλE) . (over value (+ α*(err + Δ)*E - α*Δ)) as
                        else (over eligibility (γ*λ*)) . (over value (+ α*(err + Δ)*(_eligibility as))) as

    euler :: Double
    euler = 2.71828

    initAS :: ActorState
    initAS s a = ActorState {_state=s, _action=a, _preference=0.0, _eligibility=0.0, _value=0.0}

    initASs :: Player -> [ActorState]
    initASs p = map (\s -> map (\a -> initAS s a) (allMoves s)) (playerStates p)

    eulerPref :: ActorState -> Double
    eulerPref as = euler**(_preference as)

    filterByState :: [ActorState] -> Board -> ([ActorState], Double)
    filterByState ass s = foldl (\t -> \as -> if (_state as == s) then (as:(fst t), (snd t + eulerPref as)) else t) ([], 0.0) ass

    actorPolicy :: Random.StdGen -> Board -> State Actor ActorState
    actorPolicy g s
        = do (ass, _) <- get
             return $ NPRandom.runSeed g $ NPRandom.pick $ Dist.relative probs ass'
                where
                    (nowASs, sum) = filterByState ass s
                    setProbActorState = \as -> let ep = eulerPref as in set probability (ep/(sum - ep)) as
                    appendProbsActorStates = \pas -> \as -> let as' = setProbActorState as in ((_probability as'):(fst pas), as':(snd pas))
                    (probs, ass') = foldl appendProbsActorStates ([], []) nowASs

    moveAS :: ActorState -> Player -> State
    moveAS as p = fst (move (_state as) p (_action as))

    reward :: State -> Player -> Double
    reward s p = if (notFinished s || isDraw s) 0.0 else if (isWin s p) then 1.0 else -1.0

    data Result = Result { actorState :: [ActorState]
                         , sevs :: [SEV]
                         , aOldV :: Double
                         , cOldV :: Double
                         }
    
    type ActorCritic = (Actor, Critic)

    learn :: Random.StdGen -> Board -> State (ActorCritic, ActorCritic) ()
    learn g s = do
        (a1,c1)
        as <- actorPolicy g s
        s' <- return $ moveAS as P1
        r <- return $ reward s' P1
        (actor, critic) <- get
             
        where
            (s, s', r) = 
            criticEvaluate (s,s') r 

    -- True online Sarsa(λ), with eligibility traces and with a policy using a gradient ascent distribution (Gibbs distribution)
    actorControl :: Int -> Int -> Int -> Random.StdGen -> State -> [ActorState] -> [SEV] -> Double -> [ActorState] -> [SEV] -> Double -> Result
    actorControl i tr otr g s ass sevs aOldV cOldV oass osevs oaOldV ocOldV = 
            if i mod 100 == 0 then
                if tr > otr then
                    actorControl (i + 1) (tr + r) (otr + or) g (startAgain os') ass' sevs' aOldV' cOldV' ass' sevs' aOldV' cOldV'
                else
                    actorControl (i + 1) (tr + r) (otr + or) g (startAgain os') osevs' oaOldV' ocOldV' osevs' oaOldV' ocOldV'
            else if i < 399 then 
                actorControl (i + 1) (tr + r) (otr + or) g (startAgain os') ass' sevs' aOldV' cOldV' oass' osevs' oaOldV' ocOldV'    
            else
                if tr > otr then
                    Result { actorState=ass', sevs=sevs', aOldV', cOldV'}
                else
                    Result { actorState=oass', sevs=osevs', oaOldV', ocOldV'}
        where
            as = actorPolicy g s ass
            s' = moveAS as P1
            r = reward s' P1
            oas = if isDraw s' || (isWin s' p) then else actorPolicy g s' oass
            os' = moveAS oas P2
            or = reward os' P2
            as' = actorPolicy g os' ass
            oas' = actorPolicy g (moveAS as' P1) oass
            (sevs', cOldV', cErr) = criticEvaluate sevs (_state as) (_state as') r cOldV
            (ass', aOldV') = actorEvaluation ass as as' r aOldV cErr
            (osevs', ocOldV', ocErr) = criticEvaluate sevs' (_state oas) (_state oas') or ocOldV
            (oass', oaOldV') = actorEvaluation oass oas oas' or oaOldV ocErr