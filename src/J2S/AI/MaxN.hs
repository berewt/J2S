{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}

{- |
 MaxN algorithm for 'Game'.

 MaxN is a generalization of the 'minMax' algorithm for more than 2 players,
 where we assume that each player will try to maximize it's "score".
-}
module J2S.AI.MaxN
  ( MaxNParam (MaxNParam)
  , depth
  , eval
  , maxN
  , paranoid
  ) where

import qualified Data.Functor.Foldable as FF
import qualified Data.List.NonEmpty    as NE
import qualified Data.Maybe            as M
import qualified Data.NLTree           as NL

import           Control.Lens
import           Control.Monad.Reader

import           Data.Foldable         (maximumBy)
import           Data.Function         (on)
import           Data.Monoid           (mconcat)

import           Numeric.Natural

import           J2S.AI.Types
import           J2S.Engine

-- | Parameter for a call to maxN
data MaxNParam b s
  = MaxNParam
  { -- | Number of plays in advance that we consider
    _depth     :: Natural
    -- | Evaluation function : given a board configuration, provide a score for each player
  , _eval      :: Eval b [(Player b, s)]
    -- | Strateg to order evaluations
  , _orderEval :: GlobalEval s -> GlobalEval s -> Ordering
  }

data GlobalEval v
  = GlobalEval
  { _rootPlayerScore   :: v
  , _activePlayerScore :: v
  , _otherPlayerScores :: [v]
  }

-- | Going throough a key-value list, and extract the value of a key,
-- removing the corresponding entry.
lookupAndRemove :: (MonadPlus m, Eq k) => k -> [(k, v)] -> m (v, [(k,v)])
lookupAndRemove k = let
  go FF.Nil = mzero
  go (FF.Cons h@(k',v) t)
    | k == k' = pure (v, fst t)
    | otherwise = ((h:) <$>) <$> snd t
  in FF.para go

-- | Build a global evaluation out of a list of individual evaluation for
-- the different players.
toGlobalEval :: (MonadPlus m, Eq k)
             => k -> k -> [(k, v)] -> m (GlobalEval v)
toGlobalEval rootPlayer activePlayer ss = do
  (rs, ss') <- lookupAndRemove rootPlayer ss
  (as, ss'') <- lookupAndRemove activePlayer ss'
  pure $ GlobalEval rs as $ snd <$> ss''


makeLenses ''MaxNParam
makeLenses ''GlobalEval

maxN :: (ListableActions b, Ord s, Num s, Eq (Player b))
     => Strategy (Reader (MaxNParam b s)) b
maxN b = do
  d <- asks (view depth)
  e <- asks (view eval)
  o <- asks (view orderEval)
  pure $ foldForest o e . nextPlayer <*> fromGame d $ b

foldForest :: (Game b, Ord v, Num v, Eq (Player b))
           => (GlobalEval v -> GlobalEval v -> Ordering)
           -> Eval b [(Player b, v)]
           -> Player b
           -> NE.NonEmpty (c, PlayTree b)
           -> c
foldForest o e r = let
  toValue = M.fromMaybe (error "Root player should have a score") . toGlobalEval r r . snd
  in fst . maximumBy (o `on` toValue) . fmap (foldTree o e r <$>)

foldTree :: (Game b, Eq (Player b))
         => (GlobalEval v -> GlobalEval v -> Ordering)
         -> Eval b [(Player b, v)]
         -> Player b
         -> PlayTree b
         -> [(Player b, v)]
foldTree o e r = let
  go (NL.L l) = e l
  go (NL.N c xs) = maximumBy (chooseValue o r (nextPlayer c)) xs
  in FF.cata go

chooseValue  :: Eq k
             => (GlobalEval v -> GlobalEval v -> Ordering)
             -> k
             -> k
             -> [(k, v)] -> [(k, v)] -> Ordering
chooseValue o rootPlayer activePlayer s1 s2 = either id id $ do
  gs1 <- maybe (Left GT) pure $ toGlobalEval rootPlayer activePlayer s1
  gs2 <- maybe (Left LT) pure $ toGlobalEval rootPlayer activePlayer s2
  pure $ o gs1 gs2

paranoid :: (Ord s, Num s) => GlobalEval s -> GlobalEval s -> Ordering
paranoid = let
  baseCase = view activePlayerScore
  sumCase = sum . view otherPlayerScores
  paranoidCase = view rootPlayerScore
  cs = [baseCase, sumCase, paranoidCase]
  in (mconcat .) . (sequence <$> traverse (compare `on`) cs)
