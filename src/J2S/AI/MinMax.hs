{-# LANGUAGE TemplateHaskell #-}

module J2S.AI.MinMax
  ( MinMaxParam (..)
  , depth
  , eval
  , minMax
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

data MinMaxParam b s
  = MinMaxParam
  { _depth :: Natural
  , _eval :: Eval (Player b) (End b) b s
  }

makeLenses ''MinMaxParam

minMax :: (BoardInfo b, ListableActions b, Ord s)
       => Strategy (Reader (MinMaxParam b s)) b
minMax b = do
  d <- asks (view depth)
  e <- asks (view eval)
  return $ foldForest e . nextPlayer <*> fromGame d $ b

foldForest :: (BoardInfo b, Ord s)
           => Eval (Player b) a b s
           -> Player b
           -> PlayForest (Action b) a b
           -> Action b
foldForest e p =
    fst . maximumBy (comparing snd) . fmap (fmap (e p . foldTree e))

foldTree :: (BoardInfo b, Ord s)
         => Eval (Player b) a b s
         -> PlayTree a b
         -> Either a b
foldTree e = let
  go (NL.L l) = l
  go (NL.N c xs) = maximumBy (comparing $ e (nextPlayer c)) xs
  in FF.cata go
