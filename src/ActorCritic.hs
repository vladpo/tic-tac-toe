{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE UnicodeSyntax    #-}
module ActorCritic where

import           Control.Lens                     hiding ((??))
import qualified Control.Monad.Reader.Class       as R ( MonadReader
                                                       ,ask
                                                       )
import qualified Control.Monad.State.Lazy        as ST ( MonadState
                                                        , get
                                                        , gets
                                                        , put
                                                        , modify
                                                        , state
                                                        , runState
                                                        )
import           Numeric.Probability.Distribution as Dist ( just
                                                          , relative
                                                          , (??)
                                                          , pretty
                                                          , decons
                                                          )
import qualified Numeric.Probability.Random       as NPRandom
import qualified System.Random                    as Random
import           TicTacToe                        ( Board
                                                  , Move
                                                  , Player
                                                  , Player (P1)
                                                  , allMoves
                                                  , allReactionsByMove
                                                  , move
                                                  , notFinished
                                                  , playerStates
                                                  , initBoard
                                                  , nextPlayer
                                                  , isDraw
                                                  , notWin
                                                  , XO
                                                  , XO (X)
                                                  , XO (O)
                                                  , XO (E)
                                                  )
import Debug.Trace                                as T (trace)

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

instance Eq ActorState where
  x == y = _aState x == _aState y && _aAction x == _aAction y

instance Ord ActorState where
  x `compare` y = _aValue x `compare` _aValue y
  
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
          if (_cState cs == s) then set _1 cs nowNext
          else if (_cState cs == s') then set _2 cs nowNext
               else nowNext
       ) (initCS s, initCS s') css
    
debug :: Show a => a -> a
debug a | T.trace (show a) False = undefined
debug a = a

-- True online TD(λ) with eligibility dutch-traces
criticEvaluate :: 
  Config ->
  Critic ->
  (Board, Board) ->
  Reward ->
  (Err, Critic)
criticEvaluate c (css, oldV) ss' r = (cErr, (map update css, oldV'))
  where 
    nowNext = nowNextCS css ss'
    cErr = (r + gamma c *(_cValue . snd $ nowNext))
    err = cErr - (_cValue . fst $ nowNext)
    d = (_cValue . fst $ nowNext) - oldV
    oldV' = _cValue . snd $ nowNext
    ga = (1.0 - alfa c) * gamma c * lambda c
    e = ga*(_cEligibility . fst $ nowNext) + 1.0
    update cs =
      if _cState cs == fst ss' then
        ((set cEligibility e) . (over cValue (+ (alfa c *(err + d)*e - alfa c * d)))) cs
      else
        ((over cEligibility (ga*)) . (over cValue (+ (alfa c)*(err + d)*ga*_cEligibility cs))) cs

actorEvaluate ::
  Config ->
  Actor->
  ((ActorState, Probability), ActorState) ->
  Reward ->
  Err ->
  Actor
actorEvaluate c (ass, oldV) ((as, prob), as') r cErr = (map update ass,  oldV')
  where
    err = r + gamma c *(_aValue as') - (_aValue as)
    d = _aValue as - oldV
    ga = (1 - alfa c) * gamma c * lambda c
    asE = ga*(_aEligibility as) + 1.0 - prob
    oldV' = _aValue as'
    update :: ActorState -> ActorState
    update as'' =
      if (as'' == as) then
        ((over aPreference (+ alfa c *(cErr - _aValue as + (alfa c *(err + d)*asE - alfa c *d))*asE)) . (set aEligibility asE) . (over aValue (+ (alfa c *(err + d)*asE - alfa c *d)))) as
      else ((over aPreference (+ alfa c *(cErr - _aValue as'' + alfa c *(err + d)*ga*_aEligibility as'')*ga*_aEligibility as'')) . (over aEligibility (ga*)) . (over aValue (+ alfa c *(err + d)*ga*_aEligibility as''))) as''

eulerPref :: Config -> ActorState -> Double
eulerPref c as = euler c ** _aPreference as

actorPolicy ::
  Config ->
  (Random.StdGen, [ActorState]) ->
  Board ->
  (Random.StdGen, (ActorState, Probability))
actorPolicy c (gen, ass) s = (gen', (randAS, just randAS ?? dist))
  where
    nowAS = flip filter ass $ (== s) . _aState
    dist = flip relative nowAS $ flip map nowAS (\as -> eulerPref c as / (sum . map (eulerPref c) . filter (/=as)) nowAS)
    (randAS, gen') = ST.runState (NPRandom.decons (NPRandom.pick dist)) gen

actorControl :: 
  Config ->
  (Random.StdGen, (ActorCritic, ActorCritic)) ->
  Player ->
  Board ->
  ((Player, Board), (Random.StdGen, (ActorCritic, ActorCritic)))
-- actorControl p s | T.trace (show s) False = undefined
actorControl c (gen, ((actor, critic), (opponentActor, opponentCritic))) p s = 
  if notFinished s then
    let
      (gen', (as1, prob1)) = actorPolicy c (gen, fst actor) s
      s'= fst $ move (_aState as1) p (_aAction as1)
      ((np, ns), (nGen, (as1', r))) = 
        if notFinished s' && notWin s' p then
          let
            (gen'', (opponentAS, _)) = actorPolicy c (gen', fst opponentActor) s'
            s'' = fst $ move (_aState opponentAS) (nextPlayer p) (_aAction opponentAS)
          in
            if notFinished s'' && notWin s'' (nextPlayer p) then
              let (gen''', (as1', _)) = actorPolicy c (gen'', fst actor) s''
              in ((nextPlayer p, s'), (gen''', (as1', 0.0)))
            else
              ((P1, initBoard), (gen'', (set aValue 0.0 as1, if isDraw s'' then 0.0 else -1.0)))
        else
          ((P1, initBoard), (gen', (set aValue 0.0 as1, if isDraw s' then 0.0 else 1.0)))
    in
      let
        (cErr, critic') = criticEvaluate c critic (_aState as1, _aState as1') r
        actor' = actorEvaluate c actor ((as1, prob1), as1') r cErr
      in ((np,ns), (nGen, ((actor',critic'), (opponentActor, opponentCritic))))
  else
    actorControl c (gen, ((actor, critic), (opponentActor, opponentCritic))) P1 initBoard

swap :: (a, (b, c)) -> (a, (c, b))
swap (x, (y,z)) = (x, (z,y))

learn :: 
  Config ->
  (Random.StdGen, (ActorCritic, ActorCritic)) ->
  Int ->
  Player ->
  Board ->
  (Random.StdGen, (ActorCritic, ActorCritic))
learn c t i p s =
  let
    ((p', s'), t0) = actorControl c t p s
    t1 = if p /= p' then swap t0 else t0
    ((p'', s''), t2) = actorControl c t1 p' s'
    t3 = if p' /= p'' then swap t2 else t2
  in 
    if i < 10000 then
      learn c t3 (i+1) p'' s''
    else
      if P1 /= p'' then swap t3 else t3


-- debugB :: Board -> Board
-- debugB b | T.trace (printBoard b) False = undefined
-- debugB b = b
  
-- putSpace :: Char
-- putSpace = ' '

-- printMove :: XO -> Char
-- printMove E = putSpace
-- printMove X = 'x'
-- printMove O = 'o'
    
-- printSpacedMove :: XO -> [Char]
-- printSpacedMove s = [putSpace, printMove s, putSpace]

-- printRow :: [XO] -> [Char]
-- printRow [s0, s1, s2] = printSpacedMove s0 ++ ['|'] ++ printSpacedMove s1 ++ ['|'] ++ printSpacedMove s2

-- printRowSep :: [Char]
-- printRowSep = ['\n'] ++ replicate 10 '-' ++ ['\n']

-- printBoard :: Board -> [Char]
-- printBoard [r0, r1, r2] = 
--     printRow r0 ++
--     printRowSep ++
--     printRow r1 ++
--     printRowSep ++
--     printRow r2 ++
--     replicate 2 '\n'
