module J2S.Game.Nim.AI
  ( trivialEval
  , trivialEvalP1
  , trivialEvalP2
  , TrivialValuation
  ) where

import Control.Lens

import J2S
import J2S.Game.Nim.Core

data TrivialValuation
  = Lose
  | Undecided
  | Win
  deriving (Eq, Read, Show, Ord)


trivialEval :: Player Nim -> Eval Nim TrivialValuation
trivialEval (FirstPlayer  _) = trivialEvalP1
trivialEval (SecondPlayer _) = trivialEvalP2

trivialEvalP1 :: Eval Nim TrivialValuation
trivialEvalP1 (Right _) = Undecided
trivialEvalP1 (Left e) = case view winner e of
                            (FirstPlayer _) -> Win
                            _               -> Lose

trivialEvalP2 :: Eval Nim TrivialValuation
trivialEvalP2 (Right _) = Undecided
trivialEvalP2 (Left e) = case view winner e of
                            (SecondPlayer _) -> Win
                            _                -> Lose
