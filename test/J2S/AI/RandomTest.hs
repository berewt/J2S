module J2S.AI.RandomTest
  ( randomTests
  ) where

import Control.Monad.Random

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Test.QuickCheck

import           System.Random    (mkStdGen)
import qualified J2S           as AI
import qualified J2S.Game.Mock as M

randomTests = testGroup "Test Random AI"
  [ testProperty "Random returns a listed action" randomReturnsAListedAction
  ]

randomReturnsAListedAction :: Int -> M.MockGame -> Bool
randomReturnsAListedAction s g = (`elem` (fst <$> AI.listActions g)) $ AI.rand g `evalRand` mkStdGen s

