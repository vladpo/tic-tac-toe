{-# LANGUAGE TemplateHaskell #-}
module Critic where

    import Control.Lens

    data SEV = SEV { _state :: Board
                   , _eligibility :: Double
                   , _value :: Double
                   } deriving (Show)
    makeLenses ''SEV
    type Reward = Double
    type Err = Double
    type Critic = ([SEV], Double)

    γ :: Double
    γ = 0.9

    α :: Double
    α = 0.9

    λ :: Double
    λ = 0.9

    -- True online TD(λ) with eligibility dutch-traces
    criticEvaluate :: (Board, Board) -> Reward -> State Critic Err
    criticEvaluate ss' r = state $ criticUpdate ss' r
        
    criticUpdate :: (Board, Board) -> Reward -> Critic -> (Critic, Err)
    criticUpdate ss' r (sevs, oldV) = ((map update sevs, oldV'), err)
        where 
            (now, next) = nowNextSev sevs ss'
            err = r + γ*(_value next) - (_value now)
            Δ = _value now - oldV
            oldV' = _value next
            e = (1.0 - α)*(_eligibility . fst nowNext) + 1.0
            update :: SEV -> SEV
            update sev = 
                if _state sev == s then
                    (set eligibility (γ*λ*e)) . (over value (+ α*(err + Δ)*e - α*Δ)) sev
                else
                    (over eligibility (γ*λ*)) . (over value (+ α*(err + Δ)*(_eligibility sev))) sev

    nowNextSev :: [SEV] -> (Board, Board) -> (SEV, SEV)
    nowNextSev sevs (s, s')= foldl $ \nowNext sev -> 
                                                    if (_state sev == s) then (sev, snd nowNext)
                                                    else if (_state sev == s') then (fst nowNext, sev)
                                                    else nowNext
                                        (initSev s, initSev s') sevs

    initSev :: State -> SEV
    initSev s = SEV {state=s, eligibility=0.0, value=0.0}

    initSevs :: Player -> [SEV]
    initSevs p = map initSev (aiNonTerminalStates p)