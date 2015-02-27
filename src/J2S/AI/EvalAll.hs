{-# LANGUAGE TemplateHaskell #-}

module J2S.AI.EvalAll
  ( EvalAllParam (EvalAllParam)
  , depth
  , eval
  , evalAll
  ) where

import qualified Data.Functor.Foldable as FF
import qualified Data.NLTree as NL
import qualified Data.List.NonEmpty as NE

import Control.Applicative
import Control.Lens
import Control.Monad.Reader

import Data.Foldable (maximumBy)
import Data.Ord (comparing)

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
           => (Player b -> a -> s)
           -> Player b
           -> NE.NonEmpty (c, NL.NLTree b a)
           -> c
foldForest e p =
    fst . maximumBy (comparing snd) . fmap (fmap (e p . foldTree e))

foldTree :: (BoardInfo b, Ord s)
         => (Player b -> a -> s)
         -> NL.NLTree b a
         -> a
foldTree e = let
  go (NL.L l) = l
  go (NL.N c xs) = maximumBy (comparing $ e (nextPlayer c)) xs
  in FF.cata go
