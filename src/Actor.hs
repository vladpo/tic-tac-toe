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

    -- True online Sarsa(λ), with eligibility traces and with a policy using a gradient ascent distribution (Gibbs distribution)
    actorControl :: Random.StdGen -> Player -> [ActorState] -> State -> Double -> Double -> [ActorState]
    actorControl g p ass s oldV cErr = 
        where
            as = actorPolicy g
            err = r + γ*(_value . next nowActorStatesWithSum) - (_value as))
            Δ = _value . now nowActorStatesWithSum - oldv
            decayEligibility = \gl -> gl*(_eligibility . now nowActorStatesWithSum) + 1.0 - (_probability . now nowActorStatesWithSum))
            E = decayEligibility 1.0
            γλE = decayEligibility (γ*λ)
            oldv' = _value . next nowActorStatesWithSum
            update = \as ->
                if (_state as == s && _action as == a) then
                    
                                ActorState { state=s
                                           , action=a
                                           , probability=(euler**(preference as + α*cErr*E)/(sum nowActorStatesWithSum))
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

    actorPolicy :: Random.StdGen -> State -> ActorState
    actorPolicy g s = NPRandom.runSeed g (NPRandom.pick (Dist.relative ps as))
        where
            (nowAS, sum) = filterByState ass s
            probAction = \as -> (ep/(sum - ep), _action as)
                               where ep = eulerPref as
            appendProbAction = \pas -> \as -> (p:(fst pas), a:(snd pas))
                                            where (p, a) = probAction as
            (ps, as) = foldl appendProbAction ([], []) nowAS