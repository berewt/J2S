{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

{- |
 MinMax (or minimax) algorithm for 'Game'.

 The module provides both an unpruned version 'minMax'
 and a AB-pruned version 'minMaxAB'.
 There is not reason to use the unpruned version over the pruned one.
 The unpruned version is only there for educational purpose.

 See the wikipedia [Minimax](https://en.wikipedia.org/wiki/Minimax#Minimax_algorithm_with_alternate_moves)
 webpage if you want to know momre about minimax.
-}
module J2S.AI.MinMax
  ( MinMaxParam (MinMaxParam)
  , minMax
  , minMaxAB
  ) where

import qualified Data.Foldable         as F
import qualified Data.Functor.Foldable as FF
import qualified Data.NLTree           as NL
import qualified Data.List.NonEmpty    as NE
import qualified Data.Traversable      as T

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Ord                    (comparing)

import           Numeric.Natural

import           J2S.AI.Types

-- Parameter for a min max algorithm
data MinMaxParam b s
  = MinMaxParam
  { -- | How many play in advance should we look to take a decision?
    _depth :: Natural
    -- | How do we evaluate the board configurations?
  , _eval :: Eval b s
  }

makeLenses ''MinMaxParam


minMax :: (ListableActions b, Ord s)
       => Strategy (Reader (MinMaxParam b s)) b
minMax b = do
  d <- asks (view depth)
  e <- asks (view eval)
  pure $ foldForest e . fromGame d $ b

minMaxAB :: (ListableActions b, Ord s)
         => Strategy (Reader (MinMaxParam b s)) b
minMaxAB b = do
  d <- asks (view depth)
  e <- asks (view eval)
  pure $ foldForestAB e . fromGame d $ b


-- | Flag to indicate whether we try to maimize our socre (our turn)
-- or to minimze it (opponent's turn)
data Phase = Max | Min

changePhase :: Phase -> Phase
changePhase Max = Min
changePhase Min = Max


foldForest :: Ord s => (a -> s) -> NE.NonEmpty (c, NL.NLTree b a) -> c
foldForest e = let
    evalTree = fmap $ fmap (flip runReader Min . foldTree e)
    in fst . F.maximumBy (comparing snd) . evalTree

foldTree :: (Ord s, MonadReader Phase m)
         => (a -> s) -> NL.NLTree b a -> m s
foldTree e = let
  selector Max = F.maximum
  selector Min = F.minimum
  go (NL.L l) = pure (e l)
  go (NL.N _ xs) = do
    s <- asks selector
    s <$> local changePhase (T.sequence xs)
  in FF.cata go


foldForestAB :: Ord s => (a -> s) -> NE.NonEmpty (c, NL.NLTree b a) -> c
foldForestAB e = let
    evalTree =
      fmap $ fmap (flip evalState Nothing . flip runReaderT Min . foldTreeAB e)
    in fst . F.maximumBy (comparing snd) . evalTree

foldTreeAB :: (Ord s, MonadState (Maybe s) m, MonadReader Phase m)
           => (a -> s) -> NL.NLTree b a -> m s
foldTreeAB = let
  selector Max = max
  selector Min = min
  comp Max = (<)
  comp Min = (>)
  cut phase Nothing c n = pure (selector phase c n)
  cut phase (Just cv) c n =
    if comp phase cv n
       then Left n
       else Right $ selector phase c n
  go e (NL.L l) = pure $ e l
  go _ (NL.N _ xs) = do
      p <- ask
      cutValue <- get
      put Nothing
      (h NE.:| t) <- local changePhase $ T.sequence xs
      let score = foldM (cut p cutValue) h t
      liftA2 (>>) (put . pure) pure $ either id id score
  in FF.cata . go
