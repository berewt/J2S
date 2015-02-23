{-# LANGUAGE TemplateHaskell #-}

module J2S.AI.EvalAll
  ( EvalAllParam (..)
  , depth
  , eval
  , evalAll
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader

import Data.Foldable (maximumBy)
import qualified Data.Functor.Foldable as FF
import Data.Ord (comparing)
import qualified Data.NLTree as NL

import Numeric.Natural

import J2S
import J2S.AI.Types

data EvalAllParam b s
  = EvalAllParam
  { _depth :: Natural
  , _eval :: Player b -> Eval b s
  }

makeLenses ''EvalAllParam

evalAll :: (BoardInfo b, ListableActions b, Ord s)
       => Strategy (Reader (EvalAllParam b s)) b
evalAll b = do
  d <- asks (view depth)
  e <- asks (view eval)
  return $ foldForest e . nextPlayer <*> fromGame d $ b

foldForest :: (BoardInfo b, Ord s)
           => (Player b -> Eval b s)
           -> Player b
           -> PlayForest b
           -> Action b
foldForest e p =
    fst . maximumBy (comparing snd) . fmap (fmap (e p . foldTree e))

foldTree :: (BoardInfo b, Ord s)
         => (Player b -> Eval b s)
         -> PlayTree b
         -> Either (End b) b
foldTree e = let
  go (NL.L l) = l
  go (NL.N c xs) = maximumBy (comparing $ e (nextPlayer c)) xs
  in FF.cata go
