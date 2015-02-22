{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module J2S.AI.Types
  ( PlayForest
  , PlayTree
  , Eval
  , Strategy
  , ListableActions (..)
  , fromGame
  ) where

import qualified Data.Functor.Foldable as FF
import qualified Data.List.NonEmpty as NE
import qualified Data.NLTree as NL

import Numeric.Natural

import J2S

type PlayForest m e a = NE.NonEmpty (m, PlayTree e a)
type PlayTree e a     = NL.NLTree a (Either e a)

type Eval p e a s = p -> Either e a -> s

type Strategy a m = a -> m

class ListableActions b where
  listActions :: b -> NE.NonEmpty (Action b, Either (End b) b)

fromGame :: ListableActions a
         => Natural
         -> a
         -> PlayForest (Action a) (End a) a
fromGame d = fmap (fmap $ unfoldMoves d) . listActions

unfoldMoves :: ListableActions a
            => Natural
            -> Either (End a) a
            -> PlayTree (End a) a
unfoldMoves = let
  nextBoards = fmap snd . listActions
  go 0   x         = NL.L x
  go _ l@(Left _)  = NL.L l
  go n   (Right a) = NL.N a (NE.zip (NE.repeat $ pred n) $ nextBoards a)
  in curry (FF.ana $ uncurry go)
