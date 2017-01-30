module J2S.AITest
  ( aiTests
  ) where

import Test.Framework

import J2S.AI.RandomTest

aiTests :: Test
aiTests = testGroup "AI Tests"
  [ randomTests
  ]
