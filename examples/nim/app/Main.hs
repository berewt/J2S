module Main where

import           Control.Lens
import qualified Data.List.NonEmpty as NE
import           System.Random (getStdGen)
import           Nim

main :: IO ()
main = let
  cfg = previews neh (nimConfig Human (Computer MinMax)) (6 NE.:| [6,6])
  in do
  gen <- getStdGen
  maybe (putStrLn "Configuration issue") (textNim gen) cfg
