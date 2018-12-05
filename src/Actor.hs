{-# LANGUAGE TemplateHaskell #-}
module Actor where

    import TicTacToe (playerStates)
    import Control.Lens

    type Action = Square
    data ActorState = ActorState { _state :: State
                       , _action :: Action
                       , _probability :: Double
                       , _preference :: Double
                       , _eligibility :: Double
                       , _value :: Double
                       } deriving (Show)
    makeLenses ''ActorState
    data Result = Result { _now :: ActorState
                         , _nowOthers :: [ActorState]
                         , _next :: ActorState
                         , _sum :: Double
                         } deriving (Show)
    makeLenses ''Result
    
    initAS :: ActorState
    initAS s a = ActorState {_state=s, _action=a, _preference=0.0, _eligibility=0.0, _value=0.0}

    initASs :: Player -> [ActorState]
    initASs p = map (\s -> map (\a -> initAS s a) (possibleMoves s)) (playerStates p)

    nowNextActorState :: [ActorState] -> ((State, Action), (State, Action)) -> Result
    nowNextActorState ass ((s,a), (s',a')) =
        foldl (\result -> \as -> 
                                if (_state as == s && (_action as == a)) then 
                                    set now as result
                                else if (_state as == s && (_action as /= a)) then
                                    (over nowOthers (as:)) . (over sum (+ 2.71828**(_preference as)) result
                                else if (_state as == s' && (_action as == a')) then 
                                    set next as result
              ) (Result {_now=(initAS s a), _nowOthers=[], _next=(initAS s' a'), _sum=0.0}) ass

    -- True online Sarsa(λ), with eligibility traces and with a policy using a gradient ascent distribution (Gibbs distribution)
    actorEvaluate :: [ActorState] -> (State, Action) -> (State, Action) -> Reward -> Double -> Double -> [ActorState]
    actorEvaluate ass (s,a) (s',a') oldV cErr = 
        where
            result = nowNextActorState ass ((s,a), (s',a'))
            err = r + γ*(value . next result) - (value . now result))
            Δ = value . now result - oldv
            decayEligibility = \gl -> gl*(eligibility . now result) + 1.0 - (probability . now result))
            E = decayEligibility 1.0
            γλE = decayEligibility (γ*λ)
            oldv' = value . next result
            update = \as ->
                            if state as == s then
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
                                           , probability=(probability as)
                                           , preference=(preference as)
                                           , eligibility=(γ*λ*(eligibility as))
                                           , value=(value as + α*(err + Δ)*(eligibility as))
                                           }

    actorPolicy :: [ActorState] -> (State, Action) -> (State, Action)
    actorPolicy ass (s,a) = 