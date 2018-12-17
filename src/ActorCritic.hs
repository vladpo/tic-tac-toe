{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE UnicodeSyntax    #-}
module ActorCritic ( learn
                   ) where

import           Control.Lens                     hiding ((??))
import qualified Control.Monad.Reader.Class       as R ( MonadReader
                                                       ,ask
                                                       )
import qualified Control.Monad.State.Class        as ST ( MonadState
                                                        , get
                                                        , gets
                                                        , put
                                                        , modify
                                                        , state
                                                        )
import           Numeric.Probability.Distribution as Dist ( just
                                                          , relative
                                                          , (??)
                                                          )
import qualified Numeric.Probability.Random       as NPRandom
import qualified System.Random                    as Random
import           TicTacToe                        ( Board
                                                  , Move
                                                  , Player
                                                  , allMoves
                                                  , allReactionsByMove
                                                  , move
                                                  , notFinished
                                                  , playerStates
                                                  , initBoard
                                                  , nextPlayer
                                                  , isDraw
                                                  )

type Probability = Double
type Err = Double
type Reward = Double

data Config = Config { randGen :: Random.StdGen
                     , lambda  :: Double
                     , gamma   :: Double
                     , alfa    :: Double
                     , euler   :: Double
                     }
config ::
  Random.StdGen ->
  Double ->
  Double ->
  Double ->
  Config
config gen l g a = 
  Config { randGen = gen
         , lambda = l
         , gamma = g
         , alfa = a
         , euler = 2.71828
         }

data CriticState = CS { _cState :: Board
                      , _cEligibility :: Double
                      , _cValue :: Double
                      } deriving (Show)
type Critic = ([CriticState], Double)

data ActorState = AS { _aState :: Board
                             , _aAction      :: Move
                             , _aPreference  :: Double
                             , _aEligibility :: Double
                             , _aValue       :: Double
                      } deriving (Show)
type Actor = ([ActorState], Double)
type ActorCritic = (Actor, Critic)

makeLenses ''CriticState
makeLenses ''ActorState

initCS :: 
  Board -> 
  CriticState
initCS s = CS { _cState=s
                , _cEligibility=0.0
                , _cValue=0.0
                }

initCSs :: 
  Player -> 
  [CriticState]
initCSs p = map initCS $ playerStates p

initAS ::
  Board ->
  Move ->
  ActorState
initAS s a = AS { _aState=s
                , _aAction=a
                , _aPreference=0.0
                , _aEligibility=0.0
                , _aValue=0.0
                }

initASs :: Player -> [ActorState]
initASs p = playerStates p >>= (\s -> map (initAS s) $ allMoves s)

nowNextCS :: 
  [CriticState] ->
  (Board, Board) ->
  (CriticState, CriticState)
nowNextCS css (s, s') = 
  foldl(\nowNext cs -> 
          if (_cState cs == s) then (cs, snd nowNext)
          else if (_cState cs == s') then (fst nowNext, cs)
               else nowNext
       ) (initCS s, initCS s') css
    
-- True online TD(λ) with eligibility dutch-traces
criticEvaluate :: 
  (R.MonadReader Config m, ST.MonadState (ActorCritic, ActorCritic) m) =>
  (Board, Board) ->
  Reward ->
  ((ActorCritic, ActorCritic) -> Critic) ->
  (Critic -> (ActorCritic, ActorCritic)) ->
  m Err
criticEvaluate ss' r f g = do
  c <- R.ask
  let 
    criticUpdate :: Config -> Critic -> (Err, Critic)
    criticUpdate c (css, oldV) = (err, (map update css, oldV'))
      where 
        nowNext = nowNextCS css ss'
        err = (r + gamma c *(_cValue . snd $ nowNext)) - (_cValue . fst $ nowNext)
        d = (_cValue . fst $ nowNext) - oldV
        oldV' = _cValue . snd $ nowNext
        e = (1.0 - alfa c)*(_cEligibility . fst $ nowNext) + 1.0
        ga = gamma c * lambda c
        update cs =
          if _cState cs == fst ss' then
            ((set cEligibility (ga*e)) . (over cValue (+ (alfa c *(err + d)*e - alfa c * d)))) cs
          else
            ((over cEligibility (ga*)) . (over cValue (+ (alfa c)*(err + d)*(_cEligibility cs)))) cs
  ST.state $ over _2 g . criticUpdate c . f

instance Eq ActorState where
  x == y = _aState x == _aState y && _aAction x == _aAction y

actorEvaluate ::
  (R.MonadReader Config m, ST.MonadState (ActorCritic, ActorCritic) m) =>
  ((ActorState, Probability), ActorState) ->
  Reward ->
  Err ->
  ((ActorCritic, ActorCritic) -> Actor) ->
  (Actor -> (ActorCritic, ActorCritic)) ->
  m ()
actorEvaluate ((as, prob), as') r cErr f g = do
  c <- R.ask
  let
    actorUpdate c (ass, oldV)  = (map update ass,  oldV')
      where
        err = r + gamma c *(_aValue as') - (_aValue as)
        d = _aValue as - oldV
        decayEligibility = \γλ -> γλ*(_aEligibility as) + 1.0 - prob
        e = decayEligibility 1.0
        ga = gamma c * lambda c
        γλE = decayEligibility (ga)
        oldV' = _aValue as'
        update :: ActorState -> ActorState
        update as'' =
          if (as'' == as) then
            ((over aPreference (+ alfa c *cErr*e)) . (set aEligibility γλE) . (over aValue (+ (alfa c *(err + d)*e - alfa c *d)))) as
          else ((over aEligibility (ga*)) . (over aValue (+ alfa c *(err + d)*(_aEligibility as)))) as
  ST.modify $ g . actorUpdate c . f

eulerPref :: Config -> ActorState -> Double
eulerPref c as = euler c ** _aPreference as

actorPolicyM ::
  (R.MonadReader Config m, ST.MonadState (ActorCritic, ActorCritic) m) =>
  Board ->
  ((ActorCritic, ActorCritic) -> [ActorState]) ->
  m (ActorState, Probability)
actorPolicyM s f = do
  c <- R.ask
  let 
    actorPolicy c t = (randAS, just randAS ?? dist)
      where
        nowAS = flip filter (f t) $ (== s) . _aState
        dist = flip relative nowAS $ flip map nowAS (\as -> eulerPref c as / (sum . map (eulerPref c) . filter (/=as)) nowAS)
        randAS = NPRandom.runSeed (randGen c) $ NPRandom.pick dist
  ST.gets $ actorPolicy c

actorControl :: 
  (R.MonadReader Config m, ST.MonadState (ActorCritic, ActorCritic) m) =>
  Player ->
  Board ->
  m Board
actorControl p s = 
  if notFinished s then do
    ((actor, critic), (opponentActor, opponentCritic)) <- ST.get
    (as1, prob1) <- actorPolicyM s $ fst . fst . fst
    let 
      s'= fst $ move (_aState as1) p (_aAction as1)
      mt = 
        if notFinished s' then do
          (opponentAS, _) <- actorPolicyM s' $ fst . fst . snd
          let 
            s'' = fst $ move (_aState opponentAS) (nextPlayer p) (_aAction opponentAS)
          if notFinished s'' then do
            fmap (set _2 0.0) $ actorPolicyM s'' (fst . fst . fst)
          else
            return (set aValue 0.0 as1, if isDraw s'' then 0.0 else -1.0)
        else
          return (set aValue 0.0 as1, if isDraw s' then 0.0 else 1.0)
    (as1', r) <- mt
    cErr <- criticEvaluate (_aState as1, _aState as1') r (snd . fst) (\critic' -> ((actor, critic'), (opponentActor, opponentCritic)))
    actorEvaluate ((as1, prob1), as1') r cErr (fst . fst) (\actor' -> ((actor', critic), (opponentActor, opponentCritic)))
    return s'
  else
    actorControl p initBoard

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

learn ::
  (R.MonadReader Config m, ST.MonadState (ActorCritic, ActorCritic) m) =>
  Player ->
  Board ->
  m ()
learn p s = do
  s' <- actorControl p s
  ST.modify swap
  s'' <- actorControl (nextPlayer p) s'
  ST.modify swap
  learn p s''