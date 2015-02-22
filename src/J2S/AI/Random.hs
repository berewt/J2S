module J2S.AI.Random
  ( rand
  ) where

import Control.Monad.Random

import qualified Data.Foldable as F

import J2S
import J2S.AI.Types

rand :: (BoardInfo b, ListableActions b, MonadRandom m)
     => Strategy m b
rand = uniform . fmap fst . F.toList . listActions
