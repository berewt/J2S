module J2S.AI.MinMaxTest
  ( minMaxTests
  ) where

import           Control.Monad.Reader

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2

import           Test.QuickCheck

import qualified J2S.AI        as AI
import qualified J2S.Game.Mock as M

minMaxTests = TestGroup "Test MinMax AI"
  [ testProperty "MinMax at depth 0 is a max" minMaxAtDepth0
  , testProperty "No other option lead to better result" noBetterOption
  ]

minMaxAtDepth0 :: M.MockGame -> Bool
minMaxAtDepth0 b = let
  eval = either finalBoard board
  expected = fst . (maximum `on` eval . snd)
  in minMax b `runReader` MinMaxParam 0 eval

noBetterOption :: M.MockGame
