{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}

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

data MaxNParam b s
  = MaxNParam
  { _depth     :: Natural
  , _eval      :: Eval b [(Player b, s)]
  , _orderEval :: GlobalEval s -> GlobalEval s -> Ordering
  }

data GlobalEval v
  = GlobalEval
  { _rootPlayerScore   :: v
  , _activePlayerScore :: v
  , _otherPlayerScores :: [v]
  }

lookupAndRemove :: Eq k => k -> [(k, v)] -> Maybe (v, [(k,v)])
lookupAndRemove k = let
  go FF.Nil = Nothing
  go (FF.Cons h@(k',v) t)
    | k == k' = return (v, fst t)
    | otherwise = maybe Nothing (return . fmap (h:)) (snd t)
  in FF.para go

toGlobalEval :: Eq k
             => k -> k -> [(k, v)] -> Maybe (GlobalEval v)
toGlobalEval r a ss = do
  (rs, ss') <- lookupAndRemove r ss
  (as, ss'') <- lookupAndRemove a ss'
  return $ GlobalEval rs as $ snd <$> ss''


makeLenses ''MaxNParam
makeLenses ''GlobalEval

maxN :: (ListableActions b, Ord s, Num s, Eq (Player b))
     => Strategy (Reader (MaxNParam b s)) b
maxN b = do
  d <- asks (view depth)
  e <- asks (view eval)
  o <- asks (view orderEval)
  return $ foldForest o e . nextPlayer <*> fromGame d $ b

foldForest :: (Game b, Ord v, Num v, Eq (Player b))
           => (GlobalEval v -> GlobalEval v -> Ordering)
           -> Eval b [(Player b, v)]
           -> Player b
           -> NE.NonEmpty (c, PlayTree b)
           -> c
foldForest o e r = let
  toValue = M.fromMaybe (error "Root player should have a score") . toGlobalEval r r . snd
  in fst . maximumBy (paranoid `on` toValue) . fmap (foldTree o e r <$>)

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
chooseValue o r a s1 s2= either id id $ do
  gs1 <- maybe (Left GT) return $ toGlobalEval r a s1
  gs2 <- maybe (Left LT) return $ toGlobalEval r a s2
  return $ o gs1 gs2

paranoid :: (Ord s, Num s) => GlobalEval s -> GlobalEval s -> Ordering
paranoid = let
  baseCase = view activePlayerScore
  sumCase = sum . view otherPlayerScores
  paranoidCase = view rootPlayerScore
  cs = [baseCase, sumCase, paranoidCase]
  in (mconcat .) . (sequence <$> traverse (compare `on`) cs)
