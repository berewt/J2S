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

type PlayForest b = NE.NonEmpty (Action b, PlayTree b)
type PlayTree b   = NL.NLTree b (Either (End b) b)

type Eval b s = Either (End b) b -> s

type Strategy m b = b -> m (Action b)

class ListableActions b where
  listActions :: b -> NE.NonEmpty (Action b, Either (End b) b)

fromGame :: ListableActions b
         => Natural
         -> b
         -> PlayForest b
fromGame d = fmap (fmap $ unfoldMoves d) . listActions

unfoldMoves :: ListableActions b
            => Natural
            -> Either (End b) b
            -> PlayTree b
unfoldMoves = let
  nextBoards = fmap snd . listActions
  go 0   x         = NL.L x
  go _ l@(Left _)  = NL.L l
  go n   (Right a) = NL.N a (NE.zip (NE.repeat $ pred n) $ nextBoards a)
  in curry (FF.ana $ uncurry go)
