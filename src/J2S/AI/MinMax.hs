{-# LANGUAGE TemplateHaskell #-}

module J2S.AI.MinMax
  ( MinMaxParam (MinMaxParam)
  , minMax
  ) where

import Control.Lens
import Control.Monad.Reader

import qualified Data.Foldable as F (maximum, maximumBy, minimum)
import Data.Functor ((<$>))
import qualified Data.Functor.Foldable as FF
import Data.NLTree
import qualified Data.List.NonEmpty as NE
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

foldForest :: Ord s
           => (Either a b -> s)
           -> NE.NonEmpty (c, NLTree b (Either a b))
           -> c
foldForest e = let
    evalTree = fmap $ fmap (flip runReader Min . foldTree e)
    in fst . F.maximumBy (comparing snd) . evalTree


foldTree :: Ord s
         => (Either a b -> s)
         -> NLTree b (Either a b)
         -> Reader Phase s
foldTree e = let
  selector Max = F.maximum
  selector Min = F.minimum
  go (NL.L l) = return $ e l
  go (NL.N _ xs) = do
    s <- asks selector
    s <$> withReader changePhase (T.sequence xs)
  in FF.cata go
