{-# LANGUAGE TemplateHaskell #-}
module Critic where

    import Control.Lens

    type State = Board
    data SEV = SEV { _state :: State
                   , _eligibility :: Double
                   , _value :: Double
                   } deriving (Show)
    makeLenses ''SEV
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
                                                    if (_state sev == s) then (sev, snd nowNext)
                                                    else if (_state sev == s') then (fst nowNext, sev)
                                                    else nowNext
                               ) (initSev s, initSev s') sevs
        
    -- True online TD(λ) with eligibility dutch-traces
    criticEvaluate :: [SEV] -> State -> State -> Reward -> Double -> Maybe ([SEV], Double)
    criticEvaluate sevs s s' r oldv = (map update sevs, err)
        where 
            nowNext = nowNextSev sevs s s'
            err = r + γ*(_value . snd nowNext) - (_value . fst nowNext)
            Δ = _value . fst nowNext - oldv
            oldv' = _value . snd nowNext
            e = (1.0 - α)*(_eligibility . fst nowNext) + 1.0
            update = \sev ->
                            if _state sev == s then
                                (set eligibility (γ*λ*e)) . (over value (+ α*(err + Δ)*e - α*Δ)) sev
                            else
                                (over eligibility (γ*λ*)) . (over value (+ α*(err + Δ)*(_eligibility sev))) sev