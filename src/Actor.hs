{-# LANGUAGE TemplateHaskell #-}
module Actor where

    import TicTacToe (playerStates, allReactionsByMove, allMoves)
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

    euler :: Double
    euler = 2.71828

    initAS :: ActorState
    initAS s a = ActorState {_state=s, _action=a, _preference=0.0, _eligibility=0.0, _value=0.0}

    initASs :: Player -> [ActorState]
    initASs p = map (\s -> map (\a -> initAS s a) (allMoves s)) (playerStates p)

    eulerPref :: ActorState -> Double
    eulerPref as = euler**(_preference as)

    filterByState :: [ActorState] -> State -> ([ActorState], Double)
    filterByState ass s = foldl (\t -> \as -> if (_state as == s) then (as:(fst t), (snd t + eulerPref as)) else t) ([], 0.0) ass

    -- True online Sarsa(λ), with eligibility traces and with a policy using a gradient ascent distribution (Gibbs distribution)
    actorControl :: Player -> [ActorState] -> State -> Double -> Double -> [ActorState]
    actorControl p ass s oldV cErr = 
        where
            result = filterByState ass s
            
            err = r + γ*(_value . next result) - (_value . now result))
            Δ = _value . now result - oldv
            decayEligibility = \gl -> gl*(_eligibility . now result) + 1.0 - (_probability . now result))
            E = decayEligibility 1.0
            γλE = decayEligibility (γ*λ)
            oldv' = _value . next result
            update = \as ->
                if (_state as == s && _action as == a) then
                    
                                ActorState { state=s
                                           , action=a
                                           , probability=(euler**(preference as + α*cErr*E)/(sum result))
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

    actorPolicy :: ([ActorState], Double) -> Action
    actorPolicy t = 
        where 
            ep = eulerPref as
            ps = map (\as -> ep/(snd t - ep)) (fst t)