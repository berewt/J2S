{-# LANGUAGE TypeFamilies #-}

module J2S.AI.Types
  ( PlayForest
  , PlayTree
  , Eval
  , Strategy
  , ListableActions (..)
  , fromGame
  , listActions
  ) where

import qualified Data.Functor.Foldable as FF
import qualified Data.List.NonEmpty    as NE
import qualified Data.NLTree           as NL

import           Control.Monad.Except

import           Data.Either           (partitionEithers)

import           Numeric.Natural

import           J2S.Engine


-- | A forest of 'PlayTree'
type PlayForest b = NE.NonEmpty (Action b, PlayTree b)

-- | A 'PlayTree' has action in its internal node and the result of this actions at its leaves.
type PlayTree b   = NL.NLTree b (Either (End b) b)

-- | The type of the evaluation function
type Eval b s = Either (End b) b -> s

-- | A 'Strategy' is a function that select a state for a given game state
type Strategy m b = b -> m (Action b)

-- | A 'ListableActions' is a typeclass for Game that can list their actions
class Game b => ListableActions b where

  -- | List the actions available from a given Game state
  -- Should verify:
  -- forall x. isRight $ runExcept traverse executeAction $ actions x
  actions :: b -> NE.NonEmpty (Action b)

-- | List the actions that are available from a game state,
-- paired with the resulting game state
listActions :: ListableActions b
            => b -> NE.NonEmpty (Action b, Either (End b) b)
listActions b = let
  go = NE.zip <*> NE.fromList . attachResult b . NE.toList
  in go $ actions b

attachResult :: Game b => b -> [Action b] -> [Either (End b) b]
attachResult b xs = snd . partitionEithers $ runExcept . executeAction b <$> xs


-- | Create a 'PlayForest' after `d` moves, starting from the given game state
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


