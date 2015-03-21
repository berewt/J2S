{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}

module J2S.AI.MaxN
  ( MaxNParam (MaxNParam)
  , depth
  , eval
  , maxN
  ) where

import qualified Data.Functor.Foldable as FF
import qualified Data.List.NonEmpty    as NE
import qualified Data.NLTree           as NL

import           Control.Applicative
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
  { _depth :: Natural
  , _eval  :: Eval b [(Player b, s)]
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

maxN :: (BoardInfo b, ListableActions b, Ord s, Num s, Eq (Player b))
     => Strategy (Reader (MaxNParam b s)) b
maxN b = do
  d <- asks (view depth)
  e <- asks (view eval)
  return $ foldForest e . nextPlayer <*> fromGame d $ b

foldForest :: (BoardInfo b, Ord v, Num v, Eq (Player b))
           => Eval b [(Player b, v)]
           -> Player b
           -> NE.NonEmpty (c, PlayTree b)
           -> c
foldForest e r =
  fst . maximumBy (paranoidEval r r `on` snd) . fmap (foldTree e r <$>)

foldTree :: (BoardInfo b, Ord v, Num v, Eq (Player b))
         => Eval b [(Player b, v)]
         -> Player b
         -> PlayTree b
         -> [(Player b, v)]
foldTree e r = let
  go (NL.L l) = e l
  go (NL.N c xs) = maximumBy (paranoidEval r (nextPlayer c)) xs
  in FF.cata go

paranoidEval :: (Eq k, Ord v, Num v)
             => k
             -> k
             -> [(k, v)] -> [(k, v)] -> Ordering
paranoidEval r a s1 s2= either id id $ do
  gs1 <- maybe (Left GT) return $ toGlobalEval r a s1
  gs2 <- maybe (Left LT) return $ toGlobalEval r a s2
  return $ paranoid gs1 gs2

paranoid :: (Ord s, Num s) => GlobalEval s -> GlobalEval s -> Ordering
paranoid = let
  baseCase = view activePlayerScore
  sumCase = sum . view otherPlayerScores
  paranoidCase = view rootPlayerScore
  cs = [baseCase, sumCase, paranoidCase]
  in (mconcat .) . (sequence <$> traverse (compare `on`) cs)
