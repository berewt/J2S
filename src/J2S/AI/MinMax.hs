{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module J2S.AI.MinMax
  ( MinMaxParam (MinMaxParam)
  , minMax
  , minMaxAB
  ) where

import qualified Data.Foldable as F (maximum, maximumBy, minimum)
import qualified Data.Functor.Foldable as FF
import qualified Data.NLTree as NL
import qualified Data.List.NonEmpty as NE
import qualified Data.Traversable as T

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

import Data.NLTree
import Data.Ord (comparing)

import Numeric.Natural

import J2S.AI.Types
import J2S.Engine

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

minMaxAB :: (BoardInfo b, ListableActions b, Ord s)
         => Strategy (Reader (MinMaxParam b s)) b
minMaxAB b = do
  d <- asks (view depth)
  e <- asks (view eval)
  return $ foldForestAB e . fromGame d $ b


data Phase = Max | Min

changePhase :: Phase -> Phase
changePhase Max = Min
changePhase Min = Max


foldForest :: Ord s => (a -> s) -> NE.NonEmpty (c, NLTree b a) -> c
foldForest e = let
    evalTree = fmap $ fmap (flip runReader Min . foldTree e)
    in fst . F.maximumBy (comparing snd) . evalTree

foldTree :: (Ord s, Functor m, MonadReader Phase m)
         => (a -> s) -> NLTree b a -> m s
foldTree e = let
  selector Max = F.maximum
  selector Min = F.minimum
  go (NL.L l) = return $ e l
  go (NL.N _ xs) = do
    s <- asks selector
    s <$> local changePhase (T.sequence xs)
  in FF.cata go


foldForestAB :: Ord s => (a -> s) -> NE.NonEmpty (c, NLTree b a) -> c
foldForestAB e = let
    evalTree =
      fmap $ fmap (flip evalState Nothing . flip runReaderT Min . foldTreeAB e)
    in fst . F.maximumBy (comparing snd) . evalTree

foldTreeAB :: (Ord s, MonadState (Maybe s) m, MonadReader Phase m)
           => (a -> s) -> NLTree b a -> m s
foldTreeAB = let
  selector Max = max
  selector Min = min
  comp Max = (<)
  comp Min = (>)
  cut p Nothing h t = return $ selector p h t
  cut p (Just cv) h t =
    if comp p cv t then Left t else Right $ selector p h t
  go e (NL.L l) = return $ e l
  go _ (NL.N _ xs) = let
    in do
      p <- ask
      cutValue <- get
      put Nothing
      (h NE.:| t) <-  local changePhase $ T.sequence xs
      let score = foldM (cut p cutValue) h t
      liftA2 (>>) (put . return) return $ either id id score
  in FF.cata . go

