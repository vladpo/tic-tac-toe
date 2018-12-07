{-# LANGUAGE TemplateHaskell #-}
module Actor where

    import TicTacToe (playerStates, allReactionsByMove, allMoves)
    import Control.Lens
    import Numeric.Probability.Random as NPRandom
    import Numeric.Probability.Distribution as Dist
    import qualified System.Random as Random
    import System.Random (Random, )

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

    actorPolicy :: Random.StdGen -> State -> ActorState
    actorPolicy g s = NPRandom.runSeed g (NPRandom.pick (Dist.relative ps as))
        where
            (nowAS, sum) = filterByState ass s
            probAction = \as -> (ep/(sum - ep), _action as)
                               where ep = eulerPref as
            appendProbAction = \pas -> \as -> (p:(fst pas), a:(snd pas))
                                            where (p, a) = probAction as
            (ps, as) = foldl appendProbAction ([], []) nowAS

    -- True online Sarsa(λ), with eligibility traces and with a policy using a gradient ascent distribution (Gibbs distribution)
    actorEvaluation :: [ActorState] -> ActorState -> ActorState -> Double -> Double -> [ActorState]
    actorEvaluation ass s s' oldV cErr = 
        where
            err = r + γ*(_value s') - (_value s)
            Δ = _value s - oldv
            decayEligibility = \γλ -> γλ*(_eligibility s) + 1.0 - (_probability s)
            E = decayEligibility 1.0
            γλE = decayEligibility (γ*λ)
            oldv' = _value . s'
            update = \as ->
                if (_state as == _state s && _action as == _action s) then
                                ActorState { state=s
                                           , action=a
                                           , probability=(euler**(_preference as + α*cErr*E)/(sum nowActorStatesWithSum))
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