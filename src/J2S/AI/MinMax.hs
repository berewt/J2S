{-# LANGUAGE TemplateHaskell #-}

module J2S.AI.MinMax
  ( MinMaxParam (..)
  , minMax
  ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Foldable as F (maximum, maximumBy, minimum)
import Data.Functor ((<$>))
import qualified Data.Functor.Foldable as FF
import Data.Ord (comparing)
import qualified Data.NLTree as NL
import qualified Data.Traversable as T

import Numeric.Natural

import J2S
import J2S.AI.Types

data MinMaxParam b s
  = MinMaxParam
  { _depth :: Natural
  , _eval :: Eval b s
  }

makeLenses ''MinMaxParam

minMax :: (BoardInfo b, ListableActions b, Ord s)
       => Strategy (Reader (MinMaxParam b s)) b
minMax b = do
  d <- asks (view depth)
  e <- asks (view eval)
  return $ foldForest e . fromGame d $ b

data Phase = Max | Min

changePhase :: Phase -> Phase
changePhase Max = Min
changePhase Min = Max

foldForest :: (BoardInfo b, Ord s)
           => Eval b s
           -> PlayForest b
           -> Action b
foldForest e = let
    evalTree = fmap $ fmap (flip evalState Max . foldTree e)
    in fst . F.maximumBy (comparing snd) . evalTree


foldTree :: (BoardInfo b, Ord s)
         => Eval b s
         -> PlayTree b
         -> State Phase s
foldTree e = let
  selector Max = F.maximum
  selector Min = F.minimum
  go (NL.L l) = return $ e l
  go (NL.N _ xs) = do
    s <- gets selector
    modify changePhase
    s <$> T.sequence xs
  in FF.cata go
