module Main where

import Test.Framework.Runners.Console

import J2S.AITest

main :: IO ()
main = defaultMain [aiTests]
