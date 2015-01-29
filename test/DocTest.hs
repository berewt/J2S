module Main where

import Control.Monad ((>>=))

import Control.Category ((.))

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main = glob "src/**/*.hs" >>= doctest . ("-XNoImplicitPrelude":)
